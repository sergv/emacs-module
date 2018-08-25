----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- This module defines the implementation of the 'MonadEmacs'.
----------------------------------------------------------------------------

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Emacs.Module.Monad
  ( EmacsM
  , runEmacsM
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad.Catch as Catch
import Control.Exception.Safe.Checked (MonadThrow, Throws)
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource as Resource

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Coerce
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Text.Prettyprint.Doc
import Data.Traversable
import Data.Void
import Foreign (Storable(..))
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, nullPtr)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.NonNullPtr
import qualified Data.Emacs.Module.Raw.Env as Raw
import Data.Emacs.Module.Raw.Env.Internal (Env, RawFunctionType, exportToEmacs)
import Data.Emacs.Module.Raw.Value (RawValue, GlobalRef(..))
import Data.Emacs.Module.SymbolName (SymbolName, useSymbolNameAsCString)
import Data.Emacs.Module.SymbolName.TH
import Data.Emacs.Module.Value.Internal
import Emacs.Module.Assert
import Emacs.Module.Errors
import Emacs.Module.Monad.Class

data Environment = Environment
  { eEnv           :: !Env
  , eErrorSym      :: !(NonNullPtr RawValue)
  , eErrorData     :: !(NonNullPtr RawValue)
  , eResourceState :: !Resource.InternalState
  }

newtype EmacsM s a = EmacsM { unEmacsM :: ReaderT Environment IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , Catch.MonadThrow
    , Catch.MonadCatch
    , Catch.MonadMask
    , MonadBase IO
    , MonadFix
    )

instance MonadResource (EmacsM s) where
  liftResourceT action = EmacsM $ do
    resState <- asks eResourceState
    liftBase $ runInternalState action resState

instance MonadBaseControl IO (EmacsM s) where
  type StM (EmacsM s) a = StM (ReaderT Environment IO) a
  {-# INLINE liftBaseWith #-}
  liftBaseWith f = EmacsM (liftBaseWith (\runInBase -> f (runInBase . unEmacsM)))
  {-# INLINE restoreM #-}
  restoreM x = EmacsM (restoreM x)

runEmacsM
  :: Env
  -> (forall s. EmacsM s a)
  -> IO a
runEmacsM env (EmacsM action) =
  allocaNonNull $ \pErr ->
    allocaNonNull $ \pData ->
      Exception.bracket
        Resource.createInternalState
        Resource.closeInternalState
        (\eResourceState ->
          runReaderT action Environment
            { eEnv       = env
            , eErrorSym  = pErr
            , eErrorData = pData
            , eResourceState
            })

{-# INLINE getRawValue #-}
getRawValue :: Value s -> RawValue
getRawValue = unGlobalRef . valuePayload

{-# INLINE liftIO' #-}
liftIO' :: (Env -> IO a) -> EmacsM s a
liftIO' f = EmacsM $ asks eEnv >>= liftIO . f

{-# INLINABLE makeValue #-}
-- | Protect a raw value (i.e. a plain pointer) from Emacs GC.
--
-- Users writing emacs extersions will likely have no need to
-- call this function directly.
makeValue
  :: (WithCallStack, Throws EmacsInternalError, Throws EmacsError, Throws EmacsThrow)
  => RawValue
  -> EmacsM s (Value s)
makeValue raw = do
  env <- EmacsM $ asks eEnv
  valuePayload <-
    checkExitAndRethrowInHaskell' "makeGlobalRef failed" $
      Raw.makeGlobalRef env raw
  valueReleaseHandle <- register (Raw.freeGlobalRef env valuePayload)
  pure Value{valuePayload, valueReleaseHandle}

{-# INLINABLE unpackEnumFuncallExit #-}
unpackEnumFuncallExit
  :: (MonadThrow m, Throws EmacsInternalError, WithCallStack)
  => Raw.EnumFuncallExit -> m (FuncallExit ())
unpackEnumFuncallExit (Raw.EnumFuncallExit (CInt x)) =
  case funcallExitFromNum x of
    Nothing -> Checked.throw $ mkEmacsInternalError $
      "Unknown value of enum emacs_funcall_exit:" <+> pretty x
    Just y -> pure y

nonLocalExitGet'
  :: (WithCallStack, Throws EmacsInternalError)
  => EmacsM s (FuncallExit (RawValue, RawValue))
nonLocalExitGet' = do
  Environment{eEnv, eErrorSym, eErrorData} <- EmacsM ask
  liftIO $ do
    x <- unpackEnumFuncallExit =<< Raw.nonLocalExitGet eEnv eErrorSym eErrorData
    for x $ \_ ->
      (,) <$> (peek (unNonNullPtr eErrorSym)) <*> (peek (unNonNullPtr eErrorData))

{-# INLINE nonLocalExitClear' #-}
nonLocalExitClear' :: WithCallStack => EmacsM s ()
nonLocalExitClear' = liftIO' Raw.nonLocalExitClear

{-# INLINE nonLocalExitCheck' #-}
nonLocalExitCheck'
  :: (WithCallStack, Throws EmacsInternalError)
  => EmacsM s (FuncallExit ())
nonLocalExitCheck' = liftIO' (unpackEnumFuncallExit <=< Raw.nonLocalExitCheck)


checkExitAndRethrowInHaskell
  :: (WithCallStack, Throws EmacsInternalError, Throws EmacsError, Throws EmacsThrow)
  => Doc Void -- ^ Error message
  -> EmacsM s ()
checkExitAndRethrowInHaskell errMsg = do
  x <- nonLocalExitGet'
  case x of
    FuncallExitReturn            -> pure ()
    FuncallExitSignal (sym, dat) -> do
      nonLocalExitClear'
      dat'      <- funcallPrimitiveUnchecked [esym|cons|] [sym, dat]
      formatted <- funcallPrimitiveUnchecked [esym|prin1-to-string|] [dat']
      formatRes <- nonLocalExitCheck'
      case formatRes of
        FuncallExitSignal{} -> do
          nonLocalExitClear'
          Checked.throw $ mkEmacsInternalError $
            "Failed to format Emacs error data while processing following error:" <> line <> errMsg
        FuncallExitThrow{}  -> do
          nonLocalExitClear'
          Checked.throw $ mkEmacsInternalError $
            "Failed to format Emacs error data while processing following error:" <> line <> errMsg
        FuncallExitReturn   -> do
          formatted' <- extractTextUtf8Unchecked formatted
          Checked.throw $
            mkEmacsError errMsg $
              pretty formatted'
    FuncallExitThrow (tag, value) ->
      -- NB do not clear local exit flag - we, hopefully, should exit
      -- now by unwinding full Haskell stack and the flag should be
      -- reported all the way to Emacs to show a meaningful error or
      -- do a catch in elisp.
      Checked.throw EmacsThrow
        { emacsThrowTag   = tag
        , emacsThrowValue = value
        }

{-# INLINE checkExitAndRethrowInHaskell' #-}
checkExitAndRethrowInHaskell'
  :: (WithCallStack, Throws EmacsInternalError, Throws EmacsError, Throws EmacsThrow)
  => Doc Void -- ^ Error message
  -> EmacsM s a
  -> EmacsM s a
checkExitAndRethrowInHaskell' errMsg action =
  action <* checkExitAndRethrowInHaskell errMsg

{-# INLINE internUnchecked #-}
internUnchecked :: SymbolName -> EmacsM s RawValue
internUnchecked sym =
  liftIO' $ \env -> useSymbolNameAsCString sym $ Raw.intern env

{-# INLINE funcallUnchecked #-}
funcallUnchecked :: SymbolName -> [RawValue] -> EmacsM s RawValue
funcallUnchecked name args = do
  liftIO' $ \env -> do
    fun <- useSymbolNameAsCString name $ Raw.intern env
    withArrayLen args $ \n args' ->
      Raw.funcall env fun (fromIntegral n) (mkNonNullPtr args')

{-# INLINE funcallPrimitiveUnchecked #-}
funcallPrimitiveUnchecked :: SymbolName -> [RawValue] -> EmacsM s RawValue
funcallPrimitiveUnchecked name args =
  liftIO' $ \env -> do
    fun <- useSymbolNameAsCString name $ Raw.intern env
    withArrayLen args $ \n args' ->
      Raw.funcallPrimitive env fun (fromIntegral n) (mkNonNullPtr args')

{-# INLINE typeOfUnchecked #-}
typeOfUnchecked :: Value s -> EmacsM s RawValue
typeOfUnchecked x =
  liftIO' $ \env -> Raw.typeOf env (getRawValue x)

extractTextUtf8Unchecked
  :: (WithCallStack, Throws EmacsInternalError)
  => RawValue -> EmacsM s T.Text
extractTextUtf8Unchecked =
  fmap (TE.decodeUtf8With TE.lenientDecode) . extractStringUnchecked

extractStringUnchecked
  :: (WithCallStack, Throws EmacsInternalError)
  => RawValue -> EmacsM s BS.ByteString
extractStringUnchecked x =
  liftIO' $ \env ->
    allocaNonNull $ \pSize -> do
      res  <- Raw.copyStringContents env x nullPtr pSize
      unless (Raw.isTruthy res) $
        -- Raw.nonLocalExitClear env
        Checked.throw $ mkEmacsInternalError
          "Failed to obtain size when unpacking string. Probable cause: emacs object is not a string."
      size <- fromIntegral <$> peek (unNonNullPtr pSize)
      allocaBytesNonNull size $ \pStr -> do
        copyPerformed <- Raw.copyStringContents env x (unNonNullPtr pStr) pSize
        if Raw.isTruthy copyPerformed
        then
          -- Should subtract 1 from size to avoid NULL terminator at the end.
          BS.packCStringLen (unNonNullPtr pStr, size - 1)
        else do
          Raw.nonLocalExitClear env
          Checked.throw $ mkEmacsInternalError "Failed to unpack string"

instance (Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError) => MonadEmacs EmacsM where

  type EmacsRef    EmacsM = Value
  type EmacsReturn EmacsM = EmacsRef EmacsM

  {-# INLINE produceRef #-}
  produceRef x = do
    _ <- Resource.unprotect $ valueReleaseHandle x
    pure x

  {-# INLINE nonLocalExitCheck #-}
  nonLocalExitCheck = nonLocalExitCheck'

  {-# INLINE nonLocalExitGet #-}
  nonLocalExitGet = do
    z <- nonLocalExitGet'
    for z $ \(x, y) -> (,) <$> makeValue x <*> makeValue y

  nonLocalExitSignal sym errData = do
    errData' <- funcallPrimitiveUnchecked [esym|list|] (map getRawValue errData)
    liftIO' $ \env -> Raw.nonLocalExitSignal env (getRawValue sym) errData'

  {-# INLINE nonLocalExitThrow #-}
  nonLocalExitThrow tag errData = do
    liftIO' $ \env -> Raw.nonLocalExitThrow env tag' errData'
    Checked.throw EmacsThrow
      { emacsThrowTag   = tag'
      , emacsThrowValue = errData'
      }
    where
      tag'     = getRawValue tag
      errData' = getRawValue errData

  {-# INLINE nonLocalExitClear #-}
  nonLocalExitClear = nonLocalExitClear'

  -- {-# INLINE makeGlobalRef #-}
  -- makeGlobalRef x =
  --   checkExitAndRethrowInHaskell' "makeGlobalRef failed" $
  --     liftIO' (\env -> Raw.makeGlobalRef env x)
  --
  -- {-# INLINE freeGlobalRef #-}
  -- freeGlobalRef x =
  --   checkExitAndRethrowInHaskell' "freeGlobalRef failed" $
  --     liftIO' (\env -> Raw.freeGlobalRef env x)

  {-# INLINE freeValue #-}
  freeValue :: WithCallStack => Value s -> EmacsM s ()
  freeValue = Resource.release . valueReleaseHandle

  {-# INLINE makeFunctionExtra #-}
  makeFunctionExtra
    :: forall req opt rest extra s. (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => (forall s'. EmacsFunctionExtra req opt rest extra s' EmacsM)
    -> C8.ByteString
    -> Ptr extra
    -> EmacsM s (Value s)
  makeFunctionExtra emacsFun docs extraPtr =
    makeValue =<<
    checkExitAndRethrowInHaskell' "makeFunctionExtra failed"
      (liftIO' $ \env ->
        C8.useAsCString docs $ \docs' -> do
          implementation' <- exportToEmacs implementation
          Raw.makeFunction env minArity maxArity implementation' docs' extraPtr)
    where
      (minArity, maxArity) = arities (Proxy @req) (Proxy @opt) (Proxy @rest)

      implementation :: RawFunctionType extra
      implementation env nargs argsPtr extraPtr' =
        Checked.uncheck (Proxy @UserError) $
          Exception.handle (reportAnyErrorToEmacs env) $
            Checked.handle (reportEmacsThrowToEmacs env) $ do
              res <- runEmacsM env $ do
                v <- supplyEmacsArgs (fromIntegral nargs) argsPtr makeValue (\args -> emacsFun args extraPtr')
                pure $! valuePayload v
#ifndef MODULE_ASSERTIONS
              Raw.freeGlobalRef env res
#endif
              pure $ unGlobalRef res

  {-# INLINE funcall #-}
  funcall name args =
    makeValue =<<
    checkExitAndRethrowInHaskell' ("funcall" <+> squotes (pretty name) <+> "failed")
      (funcallUnchecked name (map getRawValue args))

  {-# INLINE funcallPrimitive #-}
  funcallPrimitive name args =
    makeValue =<<
    checkExitAndRethrowInHaskell' ("funcall primitive" <+> squotes (pretty name) <+> "failed")
      (funcallPrimitiveUnchecked name (map getRawValue args))

  {-# INLINE funcallPrimitive_ #-}
  funcallPrimitive_ name args =
    checkExitAndRethrowInHaskell' ("funcall primitive" <+> squotes (pretty name) <+> "failed")
      (void $ funcallPrimitiveUnchecked name (map getRawValue args))

  {-# INLINE intern #-}
  intern sym =
    makeValue =<<
    checkExitAndRethrowInHaskell' ("intern of" <+> squotes (pretty sym) <+> "failed")
      (internUnchecked sym)

  {-# INLINE typeOf #-}
  typeOf x =
    makeValue =<<
    checkExitAndRethrowInHaskell' "typeOf failed"
      (typeOfUnchecked x)

  {-# INLINE isNotNil #-}
  isNotNil x =
    checkExitAndRethrowInHaskell' "isNotNil failed"
      (liftIO' $ \env -> Raw.isTruthy <$> Raw.isNotNil env (getRawValue x))

  {-# INLINE eq #-}
  eq x y =
    checkExitAndRethrowInHaskell' "eq failed"
      (liftIO' $ \env -> Raw.isTruthy <$> Raw.eq env (getRawValue x) (getRawValue y))


  {-# INLINE extractWideInteger #-}
  extractWideInteger x =
    checkExitAndRethrowInHaskell' "extractWideInteger failed"
      (liftIO' $ \env -> coerce (Raw.extractInteger env (getRawValue x) :: IO CIntMax))

  {-# INLINE makeWideInteger #-}
  makeWideInteger x =
    makeValue =<<
    checkExitAndRethrowInHaskell' ("makeWideInteger of" <+> pretty x <+> "failed")
      (liftIO' $ \env -> Raw.makeInteger env (CIntMax x))

  {-# INLINE extractDouble #-}
  extractDouble x =
    checkExitAndRethrowInHaskell' "extractDouble failed"
      (liftIO' $ \env -> coerce (Raw.extractFloat env (getRawValue x) :: IO CDouble))

  {-# INLINE makeDouble #-}
  makeDouble x =
    makeValue =<<
    checkExitAndRethrowInHaskell' ("makeDouble" <+> pretty x <+> "failed")
      (liftIO' $ \env -> Raw.makeFloat env (CDouble x))

  {-# INLINE extractString #-}
  extractString x =
    checkExitAndRethrowInHaskell' "extractString failed" $
      extractStringUnchecked (getRawValue x)

  {-# INLINE makeString #-}
  makeString x =
    makeValue =<<
    checkExitAndRethrowInHaskell' "makeString failed"
      (liftIO' $ \env ->
        BS.useAsCString x $ \pStr ->
          Raw.makeString env pStr (fromIntegral (BS.length x)))

  {-# INLINE extractUserPtr #-}
  extractUserPtr x =
    checkExitAndRethrowInHaskell' "extractUserPtr failed" $
      liftIO' $ \env -> Raw.getUserPtr env $ getRawValue x

  {-# INLINE makeUserPtr #-}
  makeUserPtr finaliser ptr =
    makeValue =<<
    checkExitAndRethrowInHaskell' "makeUserPtr failed"
      (liftIO' $ \env -> Raw.makeUserPtr env finaliser ptr)

  {-# INLINE assignUserPtr #-}
  assignUserPtr dest ptr =
    checkExitAndRethrowInHaskell' "assignUserPtr failed" $
      liftIO' $ \env -> Raw.setUserPtr env (getRawValue dest) ptr

  {-# INLINE extractUserPtrFinaliser #-}
  extractUserPtrFinaliser x =
    checkExitAndRethrowInHaskell' "extractUserPtrFinaliser failed" $
      liftIO' $ \env -> Raw.getUserFinaliser env $ getRawValue x

  {-# INLINE assignUserPtrFinaliser #-}
  assignUserPtrFinaliser x finaliser =
    checkExitAndRethrowInHaskell' "assignUserPtrFinaliser failed" $
      liftIO' $ \env -> Raw.setUserFinaliser env (getRawValue x) finaliser

  {-# INLINE vecGet #-}
  vecGet vec n =
    makeValue =<<
    checkExitAndRethrowInHaskell' "vecGet failed"
      (liftIO' $ \env -> Raw.vecGet env (getRawValue vec) (fromIntegral n))

  {-# INLINE vecSet #-}
  vecSet vec n x =
    checkExitAndRethrowInHaskell' "vecSet failed" $
      liftIO' $ \env -> Raw.vecSet env (getRawValue vec) (fromIntegral n) (getRawValue x)

  {-# INLINE vecSize  #-}
  vecSize vec =
    checkExitAndRethrowInHaskell' "vecSize failed" $
      liftIO' $ \env -> fromIntegral <$> Raw.vecSize env (getRawValue vec)
