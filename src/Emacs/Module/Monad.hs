----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- This module defines the implementation of the 'MonadEmacs'.
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
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
import Control.Exception.Safe.Checked (MonadThrow, Throws)
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Coerce
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Text.Prettyprint.Doc
import Data.Void
import Foreign (Storable(..))
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, nullPtr)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.Env.Internal (Env, RawFunctionType, exportToEmacs)
import qualified Data.Emacs.Module.Env.Raw as Raw
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.SymbolName (SymbolName, useSymbolNameAsCString)
import Data.Emacs.Module.SymbolName.TH
import qualified Data.Emacs.Module.Value as Emacs
import Emacs.Module.Assert
import Emacs.Module.Errors
import Emacs.Module.Monad.Class

data Environment = Environment
  { eEnv       :: !Env
  , eErrorSym  :: !(NonNullPtr Emacs.Value)
  , eErrorData :: !(NonNullPtr Emacs.Value)
  }

newtype EmacsM a = EmacsM { unEmacsM :: ReaderT Environment IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadBase IO)

instance MonadBaseControl IO EmacsM where
  type StM EmacsM a = StM (ReaderT Environment IO) a
  {-# INLINE liftBaseWith #-}
  liftBaseWith f = EmacsM (liftBaseWith (\runInBase -> f (runInBase . unEmacsM)))
  {-# INLINE restoreM #-}
  restoreM x = EmacsM (restoreM x)

runEmacsM :: Env -> EmacsM a -> IO a
runEmacsM env (EmacsM action) =
  allocaNonNull $ \pErr ->
    allocaNonNull $ \pData ->
      runReaderT action $ Environment
        { eEnv       = env
        , eErrorSym  = pErr
        , eErrorData = pData
        }

{-# INLINE liftIO' #-}
liftIO' :: (Env -> IO a) -> EmacsM a
liftIO' f = EmacsM $ asks eEnv >>= liftIO . f

instance Throws EmacsError => MonadError EmacsError EmacsM where
  throwError = EmacsM . liftIO . Checked.throw
  catchError (EmacsM action) handler = EmacsM $ do
    e <- ask
    liftIO $
      Checked.catch (runReaderT action e) ((`runReaderT` e) . unEmacsM . handler)

{-# INLINABLE unpackEnumFuncallExit #-}
unpackEnumFuncallExit
  :: (MonadThrow m, Throws EmacsInternalError)
  => Raw.EnumFuncallExit -> m (FuncallExit ())
unpackEnumFuncallExit (Raw.EnumFuncallExit (CInt x)) =
  case funcallExitFromNum x of
    Nothing -> Checked.throw $ mkEmacsInternalError $
      "Unknown value of enum emacs_funcall_exit:" <+> pretty x
    Just y -> pure y

checkExitAndRethrowInHaskell
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => Doc Void -- ^ Error message
  -> EmacsM ()
checkExitAndRethrowInHaskell errMsg = do
  x <- nonLocalExitGet
  case x of
    FuncallExitReturn            -> pure ()
    FuncallExitSignal (sym, dat) -> do
      nonLocalExitClear
      dat'      <- funcallUnchecked [esym|cons|] [sym, dat]
      formatted <- funcallUnchecked [esym|prin1-to-string|] [dat']
      formatRes <- nonLocalExitCheck
      case formatRes of
        FuncallExitSignal{} -> do
          nonLocalExitClear
          Checked.throw $ mkEmacsInternalError $
            "Failed to format Emacs error data while processing following error:" <> line <> errMsg
        FuncallExitThrow{}  -> do
          nonLocalExitClear
          Checked.throw $ mkEmacsInternalError $
            "Failed to format Emacs error data while processing following error:" <> line <> errMsg
        FuncallExitReturn   -> do
          formatted' <- extractTextUtf8Unchecked formatted
          Checked.throw $
            mkEmacsError errMsg $
              pretty $ formatted'
    FuncallExitThrow (tag, value) ->
      -- NB do not clear local exit flag - we, hopefully, should exit
      -- now by unwinding full Haskell stack and the flag should be
      -- reported all the way to Emacs to show a meaningful error or
      -- do the catch.
      Checked.throw EmacsThrow
        { emacsThrowTag   = tag
        , emacsThrowValue = value
        }

{-# INLINE checkExitAndRethrowInHaskell' #-}
checkExitAndRethrowInHaskell'
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => Doc Void -- ^ Error message
  -> EmacsM a
  -> EmacsM a
checkExitAndRethrowInHaskell' errMsg action =
  action <* checkExitAndRethrowInHaskell errMsg

{-# INLINE internUnchecked #-}
internUnchecked :: SymbolName -> EmacsM Emacs.Value
internUnchecked sym =
  liftIO' $ \env -> useSymbolNameAsCString sym $ Raw.intern env

{-# INLINE funcallUnchecked #-}
funcallUnchecked :: SymbolName -> [Emacs.Value] -> EmacsM Emacs.Value
funcallUnchecked name args =
  liftIO' $ \env -> do
    fun <- useSymbolNameAsCString name $ Raw.intern env
    withArrayLen args $ \n args' ->
      Raw.funcall env fun (fromIntegral n) (mkNonNullPtr args')

{-# INLINE typeOfUnchecked #-}
typeOfUnchecked :: Emacs.Value -> EmacsM Emacs.Value
typeOfUnchecked x =
  liftIO' $ \env -> Raw.typeOf env x

extractTextUtf8Unchecked
  :: (WithCallStack, Throws EmacsInternalError)
  => Emacs.Value -> EmacsM T.Text
extractTextUtf8Unchecked =
  fmap (TE.decodeUtf8With TE.lenientDecode) . extractStringUnchecked

extractStringUnchecked
  :: (WithCallStack, Throws EmacsInternalError)
  => Emacs.Value -> EmacsM BS.ByteString
extractStringUnchecked x = do
  liftIO' $ \env ->
    allocaNonNull $ \pSize -> do
      res  <- Raw.copyStringContents env x nullPtr pSize
      unless (Raw.isTruthy res) $ do
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

  {-# INLINE nonLocalExitCheck #-}
  nonLocalExitCheck =
    liftIO' (unpackEnumFuncallExit <=< Raw.nonLocalExitCheck)

  nonLocalExitGet = EmacsM $ do
    Environment{eEnv, eErrorSym, eErrorData} <- ask
    liftIO $ do
      x <- unpackEnumFuncallExit =<< Raw.nonLocalExitGet eEnv eErrorSym eErrorData
      traverse (\_ -> (,) <$> peek (unNonNullPtr eErrorSym) <*> peek (unNonNullPtr eErrorData)) x

  nonLocalExitSignal sym errData = do
    errData' <- funcall [esym|list|] errData
    liftIO' $ \env -> Raw.nonLocalExitSignal env sym errData'

  {-# INLINE nonLocalExitThrow #-}
  nonLocalExitThrow tag errData =
    liftIO' $ \env -> Raw.nonLocalExitThrow env tag errData

  {-# INLINE nonLocalExitClear #-}
  nonLocalExitClear =
    liftIO' Raw.nonLocalExitClear


  {-# INLINE makeGlobalRef #-}
  makeGlobalRef x =
    checkExitAndRethrowInHaskell' "makeGlobalRef failed" $
      liftIO' (\env -> Raw.makeGlobalRef env x)

  {-# INLINE freeGlobalRef #-}
  freeGlobalRef x =
    checkExitAndRethrowInHaskell' "freeGlobalRef failed" $
      liftIO' (\env -> Raw.freeGlobalRef env x)

  {-# INLINE makeFunctionExtra #-}
  makeFunctionExtra
    :: forall req opt rest extra. (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => EmacsFunctionExtra extra req opt rest
    -> C8.ByteString
    -> Ptr extra
    -> EmacsM Emacs.Value
  makeFunctionExtra emacsFun docs extraPtr =
    checkExitAndRethrowInHaskell' "makeFunctionExtra failed" $
      liftIO' $ \env ->
        C8.useAsCString docs $ \docs' -> do
          implementation' <- exportToEmacs implementation
          Raw.makeFunction env minArity maxArity implementation' docs' extraPtr
    where
      (minArity, maxArity) = arities (Proxy @req) (Proxy @opt) (Proxy @rest)

      implementation :: RawFunctionType extra
      implementation env nargs argsPtr extraPtr' =
        Checked.uncheck (Proxy @UserError) $
          Exception.handle (reportAnyErrorToEmacs env) $
            Checked.handle (reportEmacsThrowToEmacs env) $
              (supplyEmacsArgs (fromIntegral nargs) argsPtr (emacsFun env extraPtr'))

  {-# INLINE funcall #-}
  funcall name args =
    checkExitAndRethrowInHaskell' ("funcall" <+> squotes (pretty name) <+> "failed") $
      funcallUnchecked name args

  {-# INLINE intern #-}
  intern sym =
    checkExitAndRethrowInHaskell' ("intern of" <+> squotes (pretty sym) <+> "failed") $
      internUnchecked sym

  {-# INLINE typeOf #-}
  typeOf x =
    checkExitAndRethrowInHaskell' "typeOf failed" $
      typeOfUnchecked x

  {-# INLINE isNotNil #-}
  isNotNil x =
    checkExitAndRethrowInHaskell' "isNotNil failed" $
      liftIO' $ \env -> Raw.isTruthy <$> Raw.isNotNil env x

  {-# INLINE eq #-}
  eq x y =
    checkExitAndRethrowInHaskell' "eq failed" $
      liftIO' $ \env -> Raw.isTruthy <$> Raw.eq env x y


  {-# INLINE extractWideInteger #-}
  extractWideInteger x =
    checkExitAndRethrowInHaskell' "extractWideInteger failed" $
      liftIO' $ \env -> coerce (Raw.extractInteger env x :: IO CIntMax)

  {-# INLINE makeWideInteger #-}
  makeWideInteger x =
    checkExitAndRethrowInHaskell' ("makeWideInteger of" <+> pretty x <+> "failed") $
      liftIO' $ \env -> Raw.makeInteger env (CIntMax x)

  {-# INLINE extractDouble #-}
  extractDouble x =
    checkExitAndRethrowInHaskell' "extractDouble failed" $
      liftIO' $ \env -> coerce (Raw.extractFloat env x :: IO CDouble)

  {-# INLINE makeDouble #-}
  makeDouble x =
    checkExitAndRethrowInHaskell' ("makeDouble" <+> pretty x <+> "failed") $
      liftIO' $ \env -> Raw.makeFloat env (CDouble x)

  {-# INLINE extractString #-}
  extractString x =
    checkExitAndRethrowInHaskell' "extractString failed" $
      extractStringUnchecked x

  {-# INLINE makeString #-}
  makeString x =
    checkExitAndRethrowInHaskell' "makeString failed" $
      liftIO' $ \env ->
        BS.useAsCStringLen x $ \(pStr, size) ->
          Raw.makeString env pStr (fromIntegral size)


  {-# INLINE extractUserPtr #-}
  extractUserPtr x =
    checkExitAndRethrowInHaskell' "extractUserPtr failed" $
      liftIO' $ \env -> Raw.getUserPtr env x

  {-# INLINE makeUserPtr #-}
  makeUserPtr finaliser ptr =
    checkExitAndRethrowInHaskell' "makeUserPtr failed" $
      liftIO' $ \env -> Raw.makeUserPtr env finaliser ptr

  {-# INLINE assignUserPtr #-}
  assignUserPtr dest ptr =
    checkExitAndRethrowInHaskell' "assignUserPtr failed" $
      liftIO' $ \env -> Raw.setUserPtr env dest ptr

  {-# INLINE extractUserPtrFinaliser #-}
  extractUserPtrFinaliser x =
    checkExitAndRethrowInHaskell' "extractUserPtrFinaliser failed" $
      liftIO' $ \env -> Raw.getUserFinaliser env x

  {-# INLINE assignUserPtrFinaliser #-}
  assignUserPtrFinaliser x finaliser =
    checkExitAndRethrowInHaskell' "assignUserPtrFinaliser failed" $
      liftIO' $ \env -> Raw.setUserFinaliser env x finaliser

  {-# INLINE vecGet #-}
  vecGet vec n =
    checkExitAndRethrowInHaskell' "vecGet failed" $
      liftIO' $ \env -> Raw.vecGet env vec (fromIntegral n)

  {-# INLINE vecSet #-}
  vecSet vec n x =
    checkExitAndRethrowInHaskell' "vecSet failed" $
      liftIO' $ \env -> Raw.vecSet env vec (fromIntegral n) x

  {-# INLINE vecSize  #-}
  vecSize vec =
    checkExitAndRethrowInHaskell' "vecSize failed" $
      liftIO' $ \env -> fromIntegral <$> Raw.vecSize env vec
