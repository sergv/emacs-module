----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Sync.Impl
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Emacs.Module.Monad.Sync.Impl
  ( EmacsRes(..)
  , processCalls
  , callEmacs

  -- * Reexports
  , Some
  , mkSome
  ) where

import Prelude hiding (min, max)

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Some.Newtype
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
import GHC.Stack (callStack)
import Prettyprinter

import Data.Emacs.Module.Doc qualified as Doc
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
import Emacs.Module.EmacsCall
import Emacs.Module.Errors
import Foreign.Ptr.Builder as PtrBuilder

data EmacsRes s t a
  = EmacsSuccess a
  | EmacsExitSignal s
  | EmacsExitThrow t
  deriving (Functor, Foldable, Traversable)

unpackEnumFuncallExit
  :: WithCallStack
  => EnumFuncallExit -> IO (FuncallExit ())
unpackEnumFuncallExit =
  either throwIO pure . unpackEnumFuncallExitSafe

unpackEnumFuncallExitSafe
  :: WithCallStack
  => EnumFuncallExit -> Either EmacsInternalError (FuncallExit ())
unpackEnumFuncallExitSafe (EnumFuncallExit (CInt x)) =
  case funcallExitFromNum x of
    Nothing -> Left $ mkEmacsInternalError $
      "Unknown value of enum emacs_funcall_exit:" <+> pretty x
    Just y -> Right y

processCalls
  :: WithCallStack
  => Env
  -> TMQueue (Some (EmacsCall EmacsRes MVar))
  -> IO ()
processCalls env reqs =
  allocaNonNull $ \nlsErr ->
    allocaNonNull $ \nlsData ->
      let !nls = NonLocalState{nlsErr, nlsData}
          go =
            atomically (readTMQueue reqs) >>= \case
              Just x  -> withSome x (callEmacs env nls) *> go
              Nothing -> pure ()
      in go

nonLocalExitGet
  :: WithCallStack
  => Env
  -> NonLocalState
  -> IO (FuncallExit (RawValue 'Regular, RawValue 'Regular))
nonLocalExitGet env NonLocalState{nlsErr, nlsData} = do
  x <- unpackEnumFuncallExit =<< Env.nonLocalExitGet env nlsErr nlsData
  for x $ \(_ :: ()) ->
    (,) <$> peek (unNonNullPtr nlsErr) <*> peek (unNonNullPtr nlsData)

extractString
  :: WithCallStack
  => Env
  -> NonLocalState
  -> RawValue p
  -> IO (EmacsRes EmacsSignal Void BS.ByteString)
extractString env nls x = do
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
        pure $ EmacsSuccess $ BSI.BS fp (size - 1)
      else do
       nonLocalExitGet env nls >>= \ case
         FuncallExitSignal (sym, dat) -> do
           -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
           Env.nonLocalExitClear env
           emacsSignalInfo <- extractSignalInfo env sym dat
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
  => Env
  -> NonLocalState
  -> Text
  -> a
  -> IO (EmacsRes EmacsSignal Void a)
checkNonLocalExitSignal env nls errMsg res = do
  nonLocalExitGet env nls >>= \ case
    FuncallExitReturn            ->
      pure $ EmacsSuccess res
    FuncallExitSignal (sym, dat) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      emacsSignalInfo <- extractSignalInfo env sym dat
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
  => Env
  -> NonLocalState
  -> a
  -> IO (EmacsRes EmacsSignal EmacsThrow a)
checkNonLocalExitFull env nls res =
  nonLocalExitGet env nls >>= \ case
    FuncallExitReturn            ->
      pure $ EmacsSuccess res
    FuncallExitSignal (sym, dat) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      emacsSignalInfo <- extractSignalInfo env sym dat
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

extractSignalInfo :: WithCallStack => Env -> RawValue p -> RawValue 'Regular -> IO Text
extractSignalInfo env sym dat = do
  cons          <- reifySymbolUnknown env Sym.cons
  dat'          <- withPtrLenNonNull (foldMap PtrBuilder.storable $ Tuple2 (toUnknown sym, toUnknown dat)) $ \n args ->
    Env.funcallPrimitive env cons (fromIntegral n) args
  prin1ToString <- reifySymbolUnknown env Sym.prin1ToString
  formatted     <- withPtrLenNonNull (foldMap PtrBuilder.storable $ Tuple1 dat') $ \n args ->
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
extractTextUnsafe env x = TE.decodeUtf8With TE.lenientDecode <$> extractStringUnsafe env x

extractStringUnsafe
  :: WithCallStack
  => Env
  -> RawValue p
  -> IO BS.ByteString
extractStringUnsafe env x = do
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
processInput env = do
  Env.EnumProcessInputResult (CInt x) <- Env.processInput env
  case processInputResultFromNum x of
    Nothing                   ->
      throwIO $ mkEmacsInternalError $
        "Unknown value of enum emacs_process_input_result" <+> pretty x
    Just ProcessInputContinue -> pure ()
    Just ProcessInputQuit     -> throwIO EarlyTermination

data NonLocalState = NonLocalState
  { nlsErr  :: {-# UNPACK #-} !(NonNullPtr (RawValue 'Regular))
  , nlsData :: {-# UNPACK #-} !(NonNullPtr (RawValue 'Regular))
  }

callEmacs
  :: WithCallStack
  => Env
  -> NonLocalState
  -> EmacsCall EmacsRes MVar a
  -> IO ()
callEmacs env nls = \case
  MakeGlobalRef x out   -> Env.makeGlobalRef env x >>= putMVar out
  FreeGlobalRef x       -> Env.freeGlobalRef env x

  NonLocalExitCheck out ->
    Env.nonLocalExitCheck env >>= unpackEnumFuncallExit >>= putMVar out
  NonLocalExitGet out   ->
    putMVar out =<< nonLocalExitGet env nls
  NonLocalExitSignal sym dat out -> do
    listSym <- reifySymbolUnknown env Sym.list
    withPtrLenNonNull dat $ \n args -> do
      dat'            <- Env.funcallPrimitive env listSym (fromIntegral n) args
      emacsSignalInfo <- extractSignalInfo env sym dat'
      Env.nonLocalExitSignal env sym dat'
      putMVar out EmacsSignal
        { emacsSignalSym    = toUnknown sym
        , emacsSignalData   = dat'
        , emacsSignalOrigin = callStack
        , emacsSignalInfo
        }

  NonLocalExitThrow tag val -> Env.nonLocalExitThrow env tag val
  NonLocalExitClear         -> Env.nonLocalExitClear env

  MakeFunction min max impl doc out ->
    Doc.useDocAsCString doc $ \doc' -> do
      func <- Env.makeFunction env (fromIntegral min) (fromIntegral max) impl doc' (castFunPtrToPtr (unRawFunction impl))
      Env.setFunctionFinalizer env func freeHaskellFunPtrWrapped
      putMVar out func

  Funcall func args out -> do
    withPtrLenNonNull args $ \n args' ->
      putMVar out
        =<< checkNonLocalExitFull env nls
        =<< Env.funcall env func (fromIntegral n) args'

  FuncallPrimitive func args out ->
    withPtrLenNonNull args $ \n args' ->
      putMVar out
        =<< checkNonLocalExitFull env nls
        =<< Env.funcallPrimitive env func (fromIntegral n) args'

  FuncallPrimitiveUnchecked func args out ->
    withPtrLenNonNull args $ \n args' ->
      putMVar out =<< Env.funcallPrimitive env func (fromIntegral n) args'

  Intern sym out ->
    putMVar out =<< reifySymbolUnknown env sym

  TypeOf x out ->
    putMVar out =<< Env.typeOf env x

  IsNotNil x out ->
    putMVar out . Env.isTruthy =<< Env.isNotNil env x

  Eq x y out ->
    putMVar out . Env.isTruthy =<< Env.eq env x y

  ExtractInteger x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "ExtractInteger" . fromIntegral
      =<< Env.extractInteger env x

  MakeInteger x out ->
    putMVar out =<< Env.makeInteger env (fromIntegral x)

  ExtractFloat x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "ExtractFloat" . (\(CDouble y) -> y)
      =<< Env.extractFloat env x

  MakeFloat x out ->
    putMVar out =<< Env.makeFloat env (CDouble x)

  ExtractString x out ->
    putMVar out =<< extractString env nls x

  MakeString x out ->
    BSU.unsafeUseAsCStringLen x $ \(pStr, len) ->
      putMVar out =<< Env.makeString env pStr (fromIntegral len)

  GetUserPtr x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "GetUserPtr"
      =<< Env.getUserPtr env x

  MakeUserPtr fin ptr out ->
    putMVar out =<< Env.makeUserPtr env fin ptr

  SetUserPtr dest ptr out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "SetUserPtr"
      =<< Env.setUserPtr env dest ptr

  GetUserPtrFinaliser x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "GetUserPtrFinaliser"
      =<< Env.getUserFinaliser env x

  SetUserPtrFinaliser x fin out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "SetUserPtrFinaliser"
      =<< Env.setUserFinaliser env x fin

  VecGet vec n out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "VecGet"
      =<< Env.vecGet env vec (fromIntegral n)

  VecSet vec n x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "VecSet"
      =<< Env.vecSet env vec (fromIntegral n) x

  VecSize vec out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "VecSize" . fromIntegral
      =<< Env.vecSize env vec

  ProcessInput -> processInput env
