----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Async
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- This module defines an implementation of the 'MonadEmacs'. This vesrion
-- is a good default.
----------------------------------------------------------------------------

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Emacs.Module.Monad.Sync
  ( EmacsM
  , runEmacsM
  ) where

import Control.Exception
import Control.Exception qualified as Exception
import Control.Monad.Base
import Control.Monad.Catch qualified as Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.Interleave
import Control.Monad.Primitive hiding (unsafeInterleave)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce
import Data.Emacs.Module.Doc qualified as Doc
import Data.Int
import Data.Kind
import Data.Proxy
import Data.Text (Text)
import Data.Void
import Foreign.C.Types
import Foreign.Ptr
import GHC.ForeignPtr
import GHC.Stack (callStack)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.GetRawValue
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env qualified as Env
import Data.Emacs.Module.Raw.Env.Internal (Env, RawFunctionType)
import Data.Emacs.Module.Raw.Env.Internal qualified as Env
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal
import Data.Emacs.Module.Value.Internal
import Emacs.Module.Assert
import Emacs.Module.Errors
import Emacs.Module.Monad.Class
import Emacs.Module.Monad.Common as Common
import Foreign.Ptr.Builder as PtrBuilder


data Environment = Environment
  { eEnv           :: Env
  , eNonLocalState :: {-# UNPACK #-} !NonLocalState
  , eArgsCache     :: BuilderCache (RawValue 'Unknown)
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
    , Catch.MonadThrow
    , Catch.MonadCatch
    , Catch.MonadMask
    , MonadFix
    , PrimMonad
    )

instance MonadInterleave (EmacsM s) where
  {-# INLINE unsafeInterleave #-}
  unsafeInterleave (EmacsM action) = EmacsM $ do
    env <- ask
    liftBase $ unsafeInterleave $ runReaderT action env

instance MonadIO (EmacsM s) where
  {-# INLINE liftIO #-}
  liftIO = EmacsM . lift

instance MonadBase IO (EmacsM s) where
  {-# INLINE liftBase #-}
  liftBase = EmacsM . lift

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
runEmacsM eEnv (EmacsM action) =
  withNonLocalState $ \eNonLocalState ->
    withBuilderCache 8 $ \eArgsCache ->
      runReaderT action Environment { eEnv, eNonLocalState, eArgsCache }
  -- Exception.bracket
  --   (forkFinally
  --     (runReaderT action Environment { eRequests = reqs } `Exception.finally` atomically (closeTMQueue reqs))
  --     (putMVar res))
  --   killThread
  --   (\_tid -> do
  --       Exception.bracket
  --         (forkIO $
  --             forever $ do
  --               threadDelay 16_667 -- 1/60 = 0.016666s, check on each frame whether to abort.
  --               atomically $ writeTMQueue reqs $ mkSome ProcessInput)
  --         killThread
  --         (\_tid2 -> do
  --           processCalls env reqs
  --           readMVar res >>= \case
  --             Left e  -> Exception.throwIO e
  --             Right x -> pure x))

{-# INLINE withEnv #-}
withEnv :: (Env -> IO a) -> EmacsM s a
withEnv f = EmacsM $ do
  Environment{eEnv} <- ask
  liftBase (f eEnv)

{-# INLINE withEnvCache #-}
withEnvCache :: (Env -> BuilderCache (RawValue b) -> IO a) -> EmacsM s a
withEnvCache f = EmacsM $ do
  Environment{eEnv, eArgsCache} <- ask
  liftBase $ f eEnv (coerceBuilderCache eArgsCache)

handleResult :: EmacsRes EmacsSignal EmacsThrow a -> IO a
handleResult = \case
  EmacsSuccess    x -> pure x
  EmacsExitSignal e -> throwIO e
  EmacsExitThrow  e -> throwIO e

handleResultNoThrow :: EmacsRes EmacsSignal Void a -> IO a
handleResultNoThrow = \case
  EmacsSuccess    x -> pure x
  EmacsExitSignal e -> throwIO e
  EmacsExitThrow  e -> absurd e

instance MonadEmacs EmacsM Value where

  {-# INLINE makeGlobalRef #-}
  makeGlobalRef :: WithCallStack => Value s -> EmacsM s (RawValue 'Pinned)
  makeGlobalRef x = withEnv $ \env ->
    liftBase $ Env.makeGlobalRef env $ getRawValue x

  {-# INLINE freeGlobalRef #-}
  freeGlobalRef :: WithCallStack => RawValue 'Pinned -> EmacsM s ()
  freeGlobalRef x = withEnv $ \env ->
    liftBase $ Env.freeGlobalRef env x

  nonLocalExitCheck
    :: WithCallStack
    => EmacsM s (FuncallExit ())
  nonLocalExitCheck = withEnv $ \env ->
    Env.nonLocalExitCheck env >>= Common.unpackEnumFuncallExit

  nonLocalExitGet
    :: WithCallStack
    => EmacsM s (FuncallExit (Value s, Value s))
  nonLocalExitGet = EmacsM $ do
    Environment{eEnv, eNonLocalState} <- ask
    liftBase $ do
      res <- Common.nonLocalExitGet eEnv eNonLocalState
      pure $ coerce res

  nonLocalExitSignal
    :: (WithCallStack, Foldable f)
    => Value s     -- ^ Error symbol
    -> f (Value s) -- ^ Error data, will be converted to a list as Emacs API expects.
    -> EmacsM s ()
  nonLocalExitSignal sym errData = withEnvCache $ \env cache ->
    Exception.throwIO =<< Common.nonLocalExitSignal cache env callStack (coerce sym) errData'
    where
      errData' =
        foldMap (coerce (PtrBuilder.storable :: RawValue 'Regular -> PtrBuilder.Builder (RawValue 'Regular))) errData

  nonLocalExitThrow
    :: WithCallStack
    => Value s -- ^ Tag
    -> Value s -- ^ Data
    -> EmacsM s ()
  nonLocalExitThrow tag errData = withEnv $ \env -> do
    Env.nonLocalExitThrow env tag' errData'
    Exception.throwIO EmacsThrow
      { emacsThrowTag    = tag'
      , emacsThrowValue  = errData'
      , emacsThrowOrigin = callStack
      }
    where
      tag'     = getRawValue tag
      errData' = getRawValue errData

  nonLocalExitClear :: WithCallStack => EmacsM s ()
  nonLocalExitClear = withEnv Env.nonLocalExitClear

  {-# INLINE makeFunction #-}
  makeFunction
    :: forall req opt rest s. (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => (forall s'. EmacsFunction req opt rest EmacsM Value s')
    -> Doc.Doc
    -> EmacsM s (Value s)
  makeFunction emacsFun doc = withEnv $ \env -> do
    impl' <- liftBase $ Env.exportToEmacs impl
    Doc.useDocAsCString doc $ \doc' -> do
      func <- Env.makeFunction env minArity maxArity impl' doc' (castFunPtrToPtr (Env.unRawFunction impl'))
      Env.setFunctionFinalizer env func Env.freeHaskellFunPtrWrapped
      pure $ Value func
    where
      (minArity, maxArity) = arities (Proxy @req) (Proxy @opt) (Proxy @rest)

      impl :: RawFunctionType 'Unknown ()
      impl envPtr nargs argsPtr _extraPtr = do
        let env = Env.fromPtr envPtr
        Exception.handle (reportAnyErrorToEmacs env) $
          Exception.handle (reportEmacsSignalToEmacs env) $
            Exception.handle (reportEmacsThrowToEmacs env) $
              runEmacsM env $ do
                -- Force since value may contain exception.
                !res <- coerce (supplyEmacsArgs (fromIntegral nargs) argsPtr (pure . Value) emacsFun)
                pure res

  {-# INLINE funcall #-}
  funcall
    :: (WithCallStack, Foldable f)
    => Value s
    -> f (Value s)
    -> EmacsM s (Value s)
  funcall func args = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase $
          coerce . handleResult
      =<< Common.checkNonLocalExitFull (coerceBuilderCache eArgsCache) eEnv eNonLocalState
      =<< (withPtrLenNonNull (coerceBuilderCache eArgsCache) (foldMap (PtrBuilder.storable . getRawValue) args) $ \n args' ->
             Env.funcall eEnv (getRawValue func) (fromIntegral n) args')


  {-# INLINE funcallPrimitive #-}
  funcallPrimitive
    :: (WithCallStack, Foldable f)
    => Value s
    -> f (Value s)
    -> EmacsM s (Value s)
  funcallPrimitive func args = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase $
          coerce . handleResult
      =<< Common.checkNonLocalExitFull (coerceBuilderCache eArgsCache) eEnv eNonLocalState
      =<< (withPtrLenNonNull (coerceBuilderCache eArgsCache) (foldMap (PtrBuilder.storable . getRawValue) args) $ \n args' ->
            Env.funcallPrimitive eEnv (getRawValue func) (fromIntegral n) args')

  {-# INLINE funcallPrimitiveUnchecked #-}
  funcallPrimitiveUnchecked
    :: (WithCallStack, Foldable f)
    => Value s
    -> f (Value s)
    -> EmacsM s (Value s)
  funcallPrimitiveUnchecked func args =
    withEnvCache $ \env cache ->
      withPtrLenNonNull cache (foldMap (PtrBuilder.storable . getRawValue) args) $ \n args' ->
        coerce $ Env.funcallPrimitive @IO env (getRawValue func) (fromIntegral n) args'

  intern
    :: WithCallStack
    => SymbolName
    -> EmacsM s (Value s)
  intern sym = withEnv $ \env ->
    coerce $ reifySymbolUnknown env sym

  typeOf
    :: WithCallStack
    => Value s -> EmacsM s (Value s)
  typeOf x = withEnv $ \env ->
    coerce $ Env.typeOf @IO env (getRawValue x)

  {-# INLINE isNotNil #-}
  isNotNil :: WithCallStack => Value s -> EmacsM s Bool
  isNotNil x = withEnv $ \env ->
    Env.isTruthy <$> Env.isNotNil env (getRawValue x)

  eq :: Value s -> Value s -> EmacsM s Bool
  eq x y = withEnv $ \env ->
    Env.isTruthy <$> Env.eq env (getRawValue x) (getRawValue y)

  extractWideInteger :: WithCallStack => Value s -> EmacsM s Int64
  extractWideInteger x = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase
      $   handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "ExtractInteger" . fromIntegral
      =<< Env.extractInteger eEnv (getRawValue x)

  makeWideInteger :: WithCallStack => Int64 -> EmacsM s (Value s)
  makeWideInteger x = withEnv $ \env ->
    coerce $ Env.makeInteger @IO env (fromIntegral x)

  extractDouble :: WithCallStack => Value s -> EmacsM s Double
  extractDouble x = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase
      $   handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "ExtractFloat" . (\(CDouble y) -> y)
      =<< Env.extractFloat eEnv (getRawValue x)

  makeDouble :: WithCallStack => Double -> EmacsM s (Value s)
  makeDouble x = withEnv $ \env ->
    coerce $ Env.makeFloat @IO env (CDouble x)

  extractText :: WithCallStack => Value s -> EmacsM s Text
  extractText x = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase
      $   handleResultNoThrow
      =<< Common.extractText (coerceBuilderCache eArgsCache) eEnv eNonLocalState (getRawValue x)

  extractShortByteString :: WithCallStack => Value s -> EmacsM s ShortByteString
  extractShortByteString x = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase
      $   handleResultNoThrow
      =<< Common.extractShortByteString (coerceBuilderCache eArgsCache) eEnv eNonLocalState (getRawValue x)

  makeString :: WithCallStack => BS.ByteString -> EmacsM s (Value s)
  makeString x = withEnv $ \env ->
    BSU.unsafeUseAsCStringLen x $ \(pStr, len) ->
      coerce $ Env.makeString @IO env pStr (fromIntegral len)

  extractUserPtr :: WithCallStack => Value s -> EmacsM s (Ptr a)
  extractUserPtr x = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase
      $   handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "GetUserPtr"
      =<< Env.getUserPtr eEnv (getRawValue x)

  makeUserPtr
    :: WithCallStack
    => FinalizerPtr a
    -> Ptr a
    -> EmacsM s (Value s)
  makeUserPtr fin ptr = withEnv $ \env ->
    coerce $ Env.makeUserPtr @IO env fin ptr

  assignUserPtr :: WithCallStack => Value s -> Ptr a -> EmacsM s ()
  assignUserPtr dest ptr = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    -- callWithResultMayFailSignalWaitSideEffect (SetUserPtr (getRawValue dest) ptr)
    liftBase $
          handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "SetUserPtr"
      =<< Env.setUserPtr eEnv (getRawValue dest) ptr

  extractUserPtrFinaliser
    :: WithCallStack => Value s -> EmacsM s (FinalizerPtr a)
  extractUserPtrFinaliser x = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase $
          handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "GetUserPtrFinaliser"
      =<< Env.getUserFinaliser eEnv (getRawValue x)

  assignUserPtrFinaliser
    :: WithCallStack => Value s -> FinalizerPtr a -> EmacsM s ()
  assignUserPtrFinaliser x fin = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase $
          handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "SetUserPtrFinaliser"
      =<< Env.setUserFinaliser eEnv (getRawValue x) fin

  vecGet :: WithCallStack => Value s -> Int -> EmacsM s (Value s)
  vecGet vec n = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase $
          coerce . handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "VecGet"
      =<< Env.vecGet eEnv (getRawValue vec) (fromIntegral n)

  unsafeVecGet :: WithCallStack => Value s -> Int -> EmacsM s (Value s)
  unsafeVecGet vec n = EmacsM $ do
    Environment{eEnv} <- ask
    liftBase $
      coerce $
        Env.vecGet @IO eEnv (getRawValue vec) (fromIntegral n)

  vecSet
    :: WithCallStack
    => Value s -- ^ Vector
    -> Int     -- ^ Index
    -> Value s -- ^ New value
    -> EmacsM s ()
  vecSet vec n x = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase $
          handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "VecSet"
      =<< Env.vecSet eEnv (getRawValue vec) (fromIntegral n) (getRawValue x)

  vecSize :: WithCallStack => Value s -> EmacsM s Int
  vecSize vec = EmacsM $ do
    Environment{eEnv, eNonLocalState, eArgsCache} <- ask
    liftBase $
          handleResultNoThrow
      =<< checkNonLocalExitSignal (coerceBuilderCache eArgsCache) eEnv eNonLocalState "VecSize" . fromIntegral
      =<< Env.vecSize eEnv (getRawValue vec)
