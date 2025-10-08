----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Common
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}

module Emacs.Module.Monad.Common
  ( EmacsRes(..)
  , NonLocalState(..)
  , withNonLocalState
  , unpackEnumFuncallExit
  , unpackEnumFuncallExitSafe
  , Emacs.Module.Monad.Common.nonLocalExitGet
  , nonLocalExitSignal
  , extractText
  , extractShortByteString
  , extractByteString
  , checkNonLocalExitSignal
  , checkNonLocalExitFull
  , extractSignalInfo
  , extractTextUnsafe
  ) where

import Control.Exception
import Control.Monad.Primitive
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as T
import Data.Traversable
import Data.Tuple.Homogenous
import Data.Void
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import GHC.IO
import GHC.Stack (CallStack, callStack)
import Prettyprinter

#ifdef ASSERTIONS
import Data.Text.Encoding qualified as TE
import Foreign.ForeignPtr qualified as Foreign
#endif

import Data.Emacs.Module.Env.Functions
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
  , nlsSize :: {-# UNPACK #-} !(NonNullPtr CPtrdiff)
  }

withNonLocalState :: (NonLocalState -> IO a) -> IO a
withNonLocalState f =
  allocaNonNull $ \ !nlsErr ->
    allocaNonNull $ \ !nlsData ->
      allocaNonNull $ \ !nlsSize ->
        f NonLocalState{nlsErr, nlsData, nlsSize}

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

{-# INLINE nonLocalExitGet #-}
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
nonLocalExitSignal cache env !emacsSignalOrigin !sym !dat = do
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

{-# INLINE extractStringWith #-}
extractStringWith
  :: WithCallStack
  => BuilderCache (RawValue a)
  -> Env
  -> NonLocalState
  -> RawValue p
  -> (Int# -> MutableByteArray# RealWorld -> IO b)
  -> IO (EmacsRes EmacsSignal Void b)
extractStringWith cache env !nls@NonLocalState{nlsSize} !x k = do
  res <- Env.copyStringContents env x nullPtr nlsSize
  if Env.isNonTruthy res
  then do
    Env.nonLocalExitClear env
    throwIO $ mkEmacsInternalError
      "Failed to obtain size when unpacking string. Probable cause: emacs object is not a string."
  else do
    I# size# <- fromIntegral <$> peek (unNonNullPtr nlsSize)
    IO $ \s1 -> case newPinnedByteArray# size# s1 of
      (# s2, mbarr# #) -> (\kk -> kk s2) (unIO (do
        !copyPerformed <- Env.copyStringContents env x (Ptr (mutableByteArrayContents# mbarr#)) nlsSize
        if Env.isTruthy copyPerformed
        then
          EmacsSuccess <$> k size# mbarr#
        else
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
               "The copy string contents operation should have never exited via throw"))

{-# INLINE extractText #-}
extractText
  :: WithCallStack
  => BuilderCache (RawValue a)
  -> Env
  -> NonLocalState
  -> RawValue p
  -> IO (EmacsRes EmacsSignal Void Text)
extractText cache env nls x =
  extractStringWith cache env nls x $ \size# mbarr# ->
#ifdef ASSERTIONS
    do
      -- Should subtract 1 from size to avoid NULL terminator at the end.
      ptr <- Foreign.newForeignPtr_ (Ptr (mutableByteArrayContents# mbarr#))
      evaluate $ TE.decodeUtf8 $ BSI.BS ptr (I# (size# -# 1#))
#endif
#ifndef ASSERTIONS
    IO $ \s1 ->
      case unsafeFreezeByteArray# mbarr# s1 of
        (# s2, barr #) ->
          -- Should subtract 1 from size to avoid NULL terminator at the end.
          (# s2, T.Text (TA.ByteArray barr) 0 (I# (size# -# 1#)) #)
#endif

{-# INLINE extractShortByteString #-}
extractShortByteString
  :: WithCallStack
  => BuilderCache (RawValue a)
  -> Env
  -> NonLocalState
  -> RawValue p
  -> IO (EmacsRes EmacsSignal Void ShortByteString)
extractShortByteString cache env nls x =
  extractStringWith cache env nls x $ \size# mbarr# ->
    IO $ \s3 ->
      -- Should subtract 1 from size to avoid NULL terminator at the end.
      case shrinkMutableByteArray# mbarr# (size# -# 1#) s3 of
        s4 ->
          case unsafeFreezeByteArray# mbarr# s4 of
            (# s5, barr #) ->
              (# s5, SBS.SBS barr #)

{-# INLINE extractByteString #-}
extractByteString
  :: WithCallStack
  => BuilderCache (RawValue a)
  -> Env
  -> NonLocalState
  -> RawValue p
  -> IO (EmacsRes EmacsSignal Void BS.ByteString)
extractByteString cache env nls x =
  extractStringWith cache env nls x $ \size# mbarr# -> evaluate $ BSI.BS
    (ForeignPtr
      (mutableByteArrayContents# mbarr#)
      (PlainPtr mbarr#))
    -- Should subtract 1 from size to avoid NULL terminator at the end.
    (I# (size# -# 1#))

{-# INLINE checkNonLocalExitSignal #-}
checkNonLocalExitSignal
  :: WithCallStack
  => BuilderCache (RawValue b)
  -> Env
  -> NonLocalState
  -> Text
  -> a
  -> IO (EmacsRes EmacsSignal Void a)
checkNonLocalExitSignal cache env !nls !errMsg !res = do
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

{-# INLINE checkNonLocalExitFull #-}
checkNonLocalExitFull
  :: WithCallStack
  => BuilderCache (RawValue b)
  -> Env
  -> NonLocalState
  -> a
  -> IO (EmacsRes EmacsSignal EmacsThrow a)
checkNonLocalExitFull cache env !nls !res =
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
extractSignalInfo cache env !sym !dat = do
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
extractTextUnsafe env !x = do
  allocaNonNull $ \pSize -> do
    res <- Env.copyStringContents env x nullPtr pSize
    if Env.isNonTruthy res
    then do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError
        "Failed to obtain size when unpacking string. Probable cause: emacs object is not a string."
    else do
      !size@(I# size#) <- fromIntegral <$> peek (unNonNullPtr pSize)
      IO $ \s1 -> case newPinnedByteArray# size# s1 of
        (# s2, mbarr #) -> (\k -> k s2) (unIO (do
          !copyPerformed <- Env.copyStringContents env x (Ptr (mutableByteArrayContents# mbarr)) pSize
          if Env.isTruthy copyPerformed
          then
            IO $ \s3 ->
              case unsafeFreezeByteArray# mbarr s3 of
                (# s4, barr #) ->
                  -- Should subtract 1 from size to avoid NULL terminator at the end.
                  (# s4, T.Text (TA.ByteArray barr) 0 (size -  1) #)
          else do
            Env.nonLocalExitClear env
            throwIO $ mkEmacsInternalError "Failed to unpack string"))
