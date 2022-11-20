----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Common
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Emacs.Module.Monad.Common
  ( EmacsRes(..)
  , NonLocalState(..)
  , withNonLocalState
  , unpackEnumFuncallExit
  , unpackEnumFuncallExitSafe
  , Emacs.Module.Monad.Common.nonLocalExitGet
  , nonLocalExitSignal
  , extractString
  , checkNonLocalExitSignal
  , checkNonLocalExitFull
  , extractSignalInfo
  , extractTextUnsafe
  , extractStringUnsafe
  , processInput
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Traversable
import Data.Tuple.Homogenous
import Data.Void
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr
import GHC.Stack (CallStack, callStack)
import Prettyprinter

import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.Env.ProcessInput
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env (EnumFuncallExit(..))
import Data.Emacs.Module.Raw.Env qualified as Env
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Emacs.Module.Assert
import Emacs.Module.Errors
import Foreign.Ptr.Builder as PtrBuilder

data EmacsRes s t a
  = EmacsSuccess a
  | EmacsExitSignal s
  | EmacsExitThrow t
  deriving (Functor, Foldable, Traversable)

data NonLocalState = NonLocalState
  { nlsErr  :: {-# UNPACK #-} !(NonNullPtr (RawValue 'Regular))
  , nlsData :: {-# UNPACK #-} !(NonNullPtr (RawValue 'Regular))
  }

withNonLocalState :: (NonLocalState -> IO a) -> IO a
withNonLocalState f =
  allocaNonNull $ \ !nlsErr ->
    allocaNonNull $ \ !nlsData ->
      f NonLocalState{nlsErr, nlsData}

unpackEnumFuncallExit
  :: WithCallStack
  => EnumFuncallExit -> IO (FuncallExit ())
unpackEnumFuncallExit =
  either throwIO pure . unpackEnumFuncallExitSafe

unpackEnumFuncallExitSafe
  :: WithCallStack
  => EnumFuncallExit -> Either EmacsInternalError (FuncallExit ())
unpackEnumFuncallExitSafe (EnumFuncallExit (CInt !x)) =
  case funcallExitFromNum x of
    Nothing -> Left $ mkEmacsInternalError $
      "Unknown value of enum emacs_funcall_exit:" <+> pretty x
    Just y  -> Right y

nonLocalExitGet
  :: WithCallStack
  => Env
  -> NonLocalState
  -> IO (FuncallExit (RawValue 'Regular, RawValue 'Regular))
nonLocalExitGet env NonLocalState{nlsErr, nlsData} = do
  exit <- Env.nonLocalExitGet env nlsErr nlsData
  foldFuncallExitFromNum
    (unEnumFuncallExit exit)
    (throwIO $ mkEmacsInternalError $ "Unknown value of enum emacs_funcall_exit:" <+> pretty exit)
    (\x ->
      for x $ \(_ :: ()) ->
        (,) <$> peek (unNonNullPtr nlsErr) <*> peek (unNonNullPtr nlsData))
  -- x <- unpackEnumFuncallExit =<< Env.nonLocalExitGet env nlsErr nlsData
  -- for x $ \(_ :: ()) ->
  --   (,) <$> peek (unNonNullPtr nlsErr) <*> peek (unNonNullPtr nlsData)

{-# INLINE nonLocalExitSignal #-}
nonLocalExitSignal
  :: WithCallStack
  => BuilderCache (RawValue a)
  -> Env
  -> CallStack
  -> RawValue 'Unknown           -- ^ Error symbol
  -> Builder (RawValue 'Regular) -- ^ Error data
  -> IO EmacsSignal
nonLocalExitSignal !cache !env !emacsSignalOrigin !sym !dat = do
  listSym <- reifySymbolUnknown env Sym.list
  withPtrLenNonNull (coerceBuilderCache cache) dat $ \n args -> do
    dat'            <- Env.funcallPrimitive env listSym (fromIntegral n) args
    emacsSignalInfo <- extractSignalInfo cache env sym dat'
    Env.nonLocalExitSignal env sym dat'
    pure EmacsSignal
      { emacsSignalSym  = toUnknown sym
      , emacsSignalData = dat'
      , emacsSignalOrigin
      , emacsSignalInfo
      }

extractString
  :: WithCallStack
  => BuilderCache (RawValue a)
  -> Env
  -> NonLocalState
  -> RawValue p
  -> IO (EmacsRes EmacsSignal Void BS.ByteString)
extractString !cache !env !nls !x = do
  allocaNonNull $ \ !pSize -> do
    res <- Env.copyStringContents env x nullPtr pSize
    if Env.isNonTruthy res
    then do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError
        "Failed to obtain size when unpacking string. Probable cause: emacs object is not a string."
    else do
      !size          <- fromIntegral <$> peek (unNonNullPtr pSize)
      !fp            <- BSI.mallocByteString size
      !copyPerformed <- unsafeWithForeignPtr fp $ \ptr ->
        Env.copyStringContents env x (castPtr ptr) pSize
      if Env.isTruthy copyPerformed
      then
        -- Should subtract 1 from size to avoid NULL terminator at the end.
        pure $ EmacsSuccess $ BSI.BS fp (size - 1)
      else do
       nonLocalExitGet env nls >>= \case
         FuncallExitSignal (sym, dat) -> do
           -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
           Env.nonLocalExitClear env
           emacsSignalInfo <- extractSignalInfo cache env sym dat
           pure $ EmacsExitSignal $ EmacsSignal
             { emacsSignalSym    = toUnknown sym
             , emacsSignalData   = dat
             , emacsSignalOrigin = callStack
             , emacsSignalInfo
             }
         FuncallExitReturn            ->
           throwIO $ mkEmacsInternalError "Failed to unpack string"
         FuncallExitThrow{} ->
           throwIO $ mkEmacsInternalError
             "The copy string contents operation should have never exited via throw"

checkNonLocalExitSignal
  :: WithCallStack
  => BuilderCache (RawValue b)
  -> Env
  -> NonLocalState
  -> Text
  -> a
  -> IO (EmacsRes EmacsSignal Void a)
checkNonLocalExitSignal !cache !env !nls !errMsg !res = do
  nonLocalExitGet env nls >>= \ case
    FuncallExitReturn            ->
      pure $ EmacsSuccess res
    FuncallExitSignal (sym, dat) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      emacsSignalInfo <- extractSignalInfo cache env sym dat
      pure $ EmacsExitSignal $ EmacsSignal
        { emacsSignalSym    = toUnknown sym
        , emacsSignalData   = dat
        , emacsSignalOrigin = callStack
        , emacsSignalInfo
        }
    FuncallExitThrow{} ->
      throwIO $ mkEmacsInternalError $
        "The operation should have never exited via throw:" <> line <> pretty errMsg

checkNonLocalExitFull
  :: WithCallStack
  => BuilderCache (RawValue b)
  -> Env
  -> NonLocalState
  -> a
  -> IO (EmacsRes EmacsSignal EmacsThrow a)
checkNonLocalExitFull !cache !env !nls !res =
  nonLocalExitGet env nls >>= \case
    FuncallExitReturn            ->
      pure $ EmacsSuccess res
    FuncallExitSignal (sym, dat) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      emacsSignalInfo <- extractSignalInfo cache env sym dat
      pure $ EmacsExitSignal $ EmacsSignal
        { emacsSignalSym    = toUnknown sym
        , emacsSignalData   = dat
        , emacsSignalOrigin = callStack
        , emacsSignalInfo
        }
      -- -- Important to clean up so that we can still call Emacs
      -- -- functions to make nil return value, etc
      -- Env.nonLocalExitClear env
    FuncallExitThrow (tag, value) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      pure $ EmacsExitThrow $ EmacsThrow
        { emacsThrowTag    = tag
        , emacsThrowValue  = value
        , emacsThrowOrigin = callStack
        }



extractSignalInfo
  :: WithCallStack
  => BuilderCache (RawValue a) -> Env -> RawValue p -> RawValue 'Regular -> IO Text
extractSignalInfo !cache !env !sym !dat = do
  cons          <- reifySymbolUnknown env Sym.cons
  dat'          <- withPtrLenNonNull (coerceBuilderCache cache) (foldMap PtrBuilder.storable $ Tuple2 (toUnknown sym, toUnknown dat)) $ \n args ->
    Env.funcallPrimitive env cons (fromIntegral n) args
  prin1ToString <- reifySymbolUnknown env Sym.prin1ToString
  formatted     <- withPtrLenNonNull (coerceBuilderCache cache) (foldMap PtrBuilder.storable $ Tuple1 dat') $ \n args ->
    Env.funcallPrimitive env prin1ToString (fromIntegral n) args
  formatRes     <- unpackEnumFuncallExit =<< Env.nonLocalExitCheck env
  case formatRes of
    FuncallExitSignal{} -> do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError "Failed to format Emacs signal data"
    FuncallExitThrow{}  -> do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError "Failed to format Emacs signal data"
    FuncallExitReturn   ->
      extractTextUnsafe env formatted

extractTextUnsafe
  :: WithCallStack
  => Env
  -> RawValue p
  -> IO Text
extractTextUnsafe !env !x = TE.decodeUtf8With TE.lenientDecode <$> extractStringUnsafe env x

extractStringUnsafe
  :: WithCallStack
  => Env
  -> RawValue p
  -> IO BS.ByteString
extractStringUnsafe !env !x = do
  allocaNonNull $ \pSize -> do
    res <- Env.copyStringContents env x nullPtr pSize
    if Env.isNonTruthy res
    then do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError
        "Failed to obtain size when unpacking string. Probable cause: emacs object is not a string."
    else do
      size          <- fromIntegral <$> peek (unNonNullPtr pSize)
      fp            <- BSI.mallocByteString size
      copyPerformed <- unsafeWithForeignPtr fp $ \ptr ->
        Env.copyStringContents env x (castPtr ptr) pSize
      if Env.isTruthy copyPerformed
      then
        -- Should subtract 1 from size to avoid NULL terminator at the end.
        pure $ BSI.BS fp (size - 1)
      else do
        Env.nonLocalExitClear env
        throwIO $ mkEmacsInternalError "Failed to unpack string"

processInput :: Env -> IO ()
processInput !env = do
  Env.EnumProcessInputResult (CInt x) <- Env.processInput env
  case processInputResultFromNum x of
    Nothing                   ->
      throwIO $ mkEmacsInternalError $
        "Unknown value of enum emacs_process_input_result" <+> pretty x
    Just ProcessInputContinue -> pure ()
    Just ProcessInputQuit     -> throwIO EarlyTermination
