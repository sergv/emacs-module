----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
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
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Emacs.Module.Monad
  ( EmacsM
  , runEmacsM
  ) where

import Control.Exception qualified as Exception
import Control.Exception.Safe.Checked (Throws)
import Control.Exception.Safe.Checked qualified as Checked
import Control.Monad.Base
import Control.Monad.Catch qualified as Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Emacs.Module.Doc qualified as Doc
import Data.Kind
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Traversable
import Data.Void
import Foreign (Storable(..))
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr (castFunPtrToPtr)
import Foreign.Ptr (nullPtr)
import Prettyprinter

import Data.Emacs.Module.Args
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.GetRawValue
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env qualified as Raw
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Data.Emacs.Module.Value.Internal
import Emacs.Module.Assert
import Emacs.Module.Errors
import Emacs.Module.Monad.Class

data Environment = Environment
  { eEnv            :: {-# UNPACK #-} !Env
  , eErrorSym       :: {-# UNPACK #-} !(NonNullPtr RawValue)
  , eErrorData      :: {-# UNPACK #-} !(NonNullPtr RawValue)
  }

-- | Concrete monad for interacting with Emacs. It provides:
--
-- 1. Ability to call Emacs C functions and automatically rethrows any
--    errors (non-local exits) from elisp as Haskell exceptions.
-- 2. Tracks ownership of any produced Emacs values and communicates
--    that to Emacs, so that GC on Emacs side will not make any
--    values in Haskell invalid (funnily enough, this can happen!).
--
-- Parameter 's' serves to make ownership-tracking capabilities possible.
-- It's use is the same as in 'Control.Monad.ST' monad. That is, it creates
-- local threads so that no produced Emacs values can leave past 'runEmacsM'.
newtype EmacsM (s :: k) (a :: Type) = EmacsM { unEmacsM :: ReaderT Environment IO a }
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

instance MonadBaseControl IO (EmacsM s) where
  type StM (EmacsM s) a = StM (ReaderT Environment IO) a
  {-# INLINE liftBaseWith #-}
  liftBaseWith f = EmacsM (liftBaseWith (\runInBase -> f (runInBase . unEmacsM)))
  {-# INLINE restoreM #-}
  restoreM x = EmacsM (restoreM x)

-- | Execute emacs interaction session using an environment supplied by Emacs.
runEmacsM
  :: WithCallStack
  => Env
  -> (forall s. EmacsM s a)
  -> IO a
runEmacsM env (EmacsM action) =
  allocaNonNull $ \pErr ->
    allocaNonNull $ \pData -> do
      runReaderT action Environment
        { eEnv       = env
        , eErrorSym  = pErr
        , eErrorData = pData
        }

{-# INLINE liftIO' #-}
liftIO' :: (Env -> IO a) -> EmacsM s a
liftIO' f = EmacsM $ asks eEnv >>= liftIO . f

instance (Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError) => MakeEmacsRef RawValue EmacsM where
  makeEmacsRef = pure . Value

instance (Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError) => MakeEmacsRef GlobalRef EmacsM where
  makeEmacsRef = pure . Value . unGlobalRef

{-# INLINABLE unpackEnumFuncallExit #-}
unpackEnumFuncallExit
  :: (WithCallStack, Catch.MonadThrow m, Throws EmacsInternalError)
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
    for x $ \() ->
      (,) <$> peek (unNonNullPtr eErrorSym) <*> peek (unNonNullPtr eErrorData)

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
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      nonLocalExitClear'
      dat'      <- funcallPrimitiveUnchecked Sym.cons [sym, dat]
      formatted <- funcallPrimitiveUnchecked Sym.prin1ToString [dat']
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
    FuncallExitThrow (tag, value) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      nonLocalExitClear'
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
checkExitAndRethrowInHaskell' errMsg action = do
  action <* checkExitAndRethrowInHaskell errMsg

{-# INLINE funcallUnchecked #-}
funcallUnchecked
  :: (UseSymbolName a, GetRawValue (ReifiedSymbol a))
  => SymbolName a -> [RawValue] -> EmacsM s RawValue
funcallUnchecked name args = do
  res <- liftIO' $ \env -> do
    fun <- reifySymbol env name
    withArrayLen args $ \n args' ->
      Raw.funcall env (getRawValue fun) (fromIntegral n) (mkNonNullPtr args')
  pure res

{-# INLINE funcallPrimitiveUnchecked #-}
funcallPrimitiveUnchecked
  :: (UseSymbolName a, GetRawValue (ReifiedSymbol a))
  => SymbolName a -> [RawValue] -> EmacsM s RawValue
funcallPrimitiveUnchecked name args =
  liftIO' $ \env -> do
    fun <- reifySymbol env name
    withArrayLen args $ \n args' ->
      Raw.funcallPrimitive env (getRawValue fun) (fromIntegral n) (mkNonNullPtr args')

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

  type EmacsRef EmacsM = Value

  {-# INLINE nonLocalExitCheck #-}
  nonLocalExitCheck = nonLocalExitCheck'

  {-# INLINE nonLocalExitGet #-}
  nonLocalExitGet = do
    z <- nonLocalExitGet'
    for z $ \(x, y) -> (,) <$> makeEmacsRef x <*> makeEmacsRef y

  nonLocalExitSignal sym errData = do
    errData' <- funcallPrimitiveUnchecked (mkSymbolNameUnsafe# "list"#) (map getRawValue errData)
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

  {-# INLINE makeFunction #-}
  makeFunction
    :: forall req opt rest s. (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => (forall s'.
         (Throws EmacsInternalError, Throws EmacsError, Throws EmacsThrow, Throws UserError) =>
         EmacsFunction req opt rest s' EmacsM)
    -> Doc.Doc
    -> EmacsM s (Value s)
  makeFunction emacsFun doc =
    makeEmacsRef =<<
    checkExitAndRethrowInHaskell' "makeFunction failed"
      (liftIO' $ \env ->
        Doc.useDocAsCString doc $ \docs' -> do
          (implementationPtr :: RawFunction ()) <- exportToEmacs implementation
          func <- Raw.makeFunction env minArity maxArity implementationPtr docs' (castFunPtrToPtr (unRawFunction implementationPtr))
          Raw.setFunctionFinalizer env func freeHaskellFunPtrWrapped
          pure func)
    where
      (minArity, maxArity) = arities (Proxy @req) (Proxy @opt) (Proxy @rest)

      implementation :: RawFunctionType ()
      implementation env nargs argsPtr _extraPtr =
        Checked.uncheck (Proxy @UserError) $
          Exception.handle (reportAnyErrorToEmacs env) $
            Checked.handle (reportEmacsThrowToEmacs env) $ do
              runEmacsM env $ do
                v <- supplyEmacsArgs (fromIntegral nargs) argsPtr makeEmacsRef emacsFun
                pure $! unValue v

  {-# INLINE funcall #-}
  funcall
    :: (WithCallStack, Pretty a, UseSymbolName a)
    => SymbolName a
    -> [EmacsRef EmacsM s]
    -> EmacsM s (EmacsRef EmacsM s)
  funcall name args =
    makeEmacsRef =<<
    checkExitAndRethrowInHaskell' ("funcall" <+> squotes (pretty name) <+> "failed")
      (funcallUnchecked name (map getRawValue args))

  {-# INLINE funcallPrimitive #-}
  funcallPrimitive
    :: (WithCallStack, Pretty a, UseSymbolName a)
    => SymbolName a
    -> [EmacsRef EmacsM s]
    -> EmacsM s (EmacsRef EmacsM s)
  funcallPrimitive name args =
    makeEmacsRef =<<
    checkExitAndRethrowInHaskell' ("funcall primitive" <+> squotes (pretty name) <+> "failed")
      (funcallPrimitiveUnchecked name (map getRawValue args))

  {-# INLINE funcallPrimitive_ #-}
  funcallPrimitive_
    :: (WithCallStack, Pretty a, UseSymbolName a)
    => SymbolName a
    -> [EmacsRef EmacsM s]
    -> EmacsM s ()
  funcallPrimitive_ name args =
    void $
      checkExitAndRethrowInHaskell' ("funcall primitive" <+> squotes (pretty name) <+> "failed")
        (funcallPrimitiveUnchecked name (map getRawValue args))

  {-# INLINE intern #-}
  intern sym =
    makeEmacsRef =<< liftIO' (\env -> reifySymbol env sym)

  {-# INLINE typeOf #-}
  typeOf x =
    makeEmacsRef =<<
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
    makeEmacsRef =<<
    checkExitAndRethrowInHaskell' ("makeWideInteger of" <+> pretty x <+> "failed")
      (liftIO' $ \env -> Raw.makeInteger env (CIntMax x))

  {-# INLINE extractDouble #-}
  extractDouble x =
    checkExitAndRethrowInHaskell' "extractDouble failed"
      (liftIO' $ \env -> coerce (Raw.extractFloat env (getRawValue x) :: IO CDouble))

  {-# INLINE makeDouble #-}
  makeDouble x =
    makeEmacsRef =<<
    checkExitAndRethrowInHaskell' ("makeDouble" <+> pretty x <+> "failed")
      (liftIO' $ \env -> Raw.makeFloat env (CDouble x))

  {-# INLINE extractString #-}
  extractString x =
    checkExitAndRethrowInHaskell' "extractString failed" $
      extractStringUnchecked (getRawValue x)

  {-# INLINE makeString #-}
  makeString x =
    makeEmacsRef =<<
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
    makeEmacsRef =<<
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
    makeEmacsRef =<<
    checkExitAndRethrowInHaskell' "vecGet failed"
      (liftIO' $ \env -> Raw.vecGet env (getRawValue vec) (fromIntegral n))

  {-# INLINE vecSet #-}
  vecSet vec n x =
    checkExitAndRethrowInHaskell' "vecSet failed" $
      liftIO' $ \env -> Raw.vecSet env (getRawValue vec) (fromIntegral n) (getRawValue x)

  {-# INLINE vecSize  #-}
  vecSize :: WithCallStack => Value s -> EmacsM s Int
  vecSize vec =
    checkExitAndRethrowInHaskell' "vecSize failed" $
      liftIO' $ \env -> fromIntegral <$> Raw.vecSize env (getRawValue vec)
