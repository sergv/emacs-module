----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Sync
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- This module defines an implementation of the 'MonadEmacs'. This vesrion
-- is a good default.
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
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

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Emacs.Module.Monad.Sync
  ( EmacsM
  , runEmacsM
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception qualified as Exception
import Control.Monad.Base
import Control.Monad.Catch qualified as Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce
import Data.Emacs.Module.Doc qualified as Doc
import Data.Int
import Data.Kind
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Void
import Foreign (Storable(..))
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castFunPtrToPtr, nullPtr, castPtr)
import Foreign.Ptr.Builder qualified as PtrBuilder
import GHC.ForeignPtr
import GHC.Stack (callStack)
import Prettyprinter
import System.IO.Unsafe

import Data.Emacs.Module.Args
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.GetRawValue
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env qualified as Raw
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Data.Emacs.Module.Value.Internal
import Emacs.Module.Assert
import Emacs.Module.EmacsCall
import Emacs.Module.Errors
import Emacs.Module.Monad.Class
import Emacs.Module.Monad.Sync.Impl

newtype Environment = Environment
  { eRequests :: TMQueue (Some (EmacsCall EmacsRes MVar))
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
runEmacsM env (EmacsM action) = do
  reqs <- newTMQueueIO
  res  <- newEmptyMVar
  Exception.bracket
    (forkFinally
      (runReaderT action Environment { eRequests = reqs } `Exception.finally` atomically (closeTMQueue reqs))
      (putMVar res))
    killThread
    (\_tid -> do
      processCalls env reqs
      readMVar res >>= \case
        Left e  -> Exception.throwIO e
        Right x -> pure x)

callNoResult
  :: WithCallStack
  => EmacsCall EmacsRes MVar a
  -> EmacsM s ()
callNoResult req = do
  reqs <- EmacsM $ asks eRequests
  liftIO $
    atomically $ writeTMQueue reqs $ mkSome req

callWithResult
  :: WithCallStack
  => (MVar a -> EmacsCall EmacsRes MVar a)
  -> EmacsM s a
callWithResult mkReq = do
  reqs <- EmacsM $ asks eRequests
  liftIO $ do
    res <- newEmptyMVar
    atomically $ writeTMQueue reqs $ mkSome $ mkReq res
    unsafeInterleaveIO $ readMVar res

callWithResultMayFailSignal
  :: WithCallStack
  => (MVar (EmacsRes EmacsSignal Void a) -> EmacsCall EmacsRes MVar (EmacsRes EmacsSignal Void a))
  -> EmacsM s a
callWithResultMayFailSignal mkReq = do
  reqs <- EmacsM $ asks eRequests
  liftIO $ do
    res <- newEmptyMVar
    atomically $ writeTMQueue reqs $ mkSome $ mkReq res
    unsafeInterleaveIO $ handleEmacsResSignal =<< readMVar res

handleEmacsResSignal
  :: WithCallStack
  => EmacsRes EmacsSignal Void a
  -> IO a
handleEmacsResSignal = \case
  EmacsExitSignal e -> Exception.throwIO e { emacsSignalOrigin = callStack }
  EmacsExitThrow x  -> absurd x
  EmacsSuccess x    -> pure x

callWithResultMayFailThrow
  :: WithCallStack
  => (MVar (EmacsRes EmacsSignal EmacsThrow a) -> EmacsCall EmacsRes MVar (EmacsRes EmacsSignal EmacsThrow a))
  -> EmacsM s a
callWithResultMayFailThrow mkReq = do
  reqs <- EmacsM $ asks eRequests
  liftIO $ do
    res <- newEmptyMVar
    atomically $ writeTMQueue reqs $ mkSome $ mkReq res
    unsafeInterleaveIO $ handleEmacsResThrow =<< readMVar res

handleEmacsResThrow
  :: WithCallStack
  => EmacsRes EmacsSignal EmacsThrow a
  -> IO a
handleEmacsResThrow = \case
  EmacsExitSignal e -> Exception.throwIO e { emacsSignalOrigin = callStack }
  EmacsExitThrow e  -> Exception.throwIO e { emacsThrowOrigin  = callStack }
  EmacsSuccess x    -> pure x

instance MonadEmacs EmacsM where

  type EmacsRef EmacsM = Value

  {-# INLINE makeGlobalRef #-}
  makeGlobalRef :: WithCallStack => Value s -> EmacsM s (RawValue 'Pinned)
  makeGlobalRef = callWithResult . MakeGlobalRef . getRawValue

  {-# INLINE freeGlobalRef #-}
  freeGlobalRef :: WithCallStack => RawValue 'Pinned -> EmacsM s ()
  freeGlobalRef = callNoResult . FreeGlobalRef

  nonLocalExitCheck
    :: WithCallStack
    => EmacsM s (FuncallExit ())
  nonLocalExitCheck = callWithResult NonLocalExitCheck

  nonLocalExitGet
    :: WithCallStack
    => EmacsM s (FuncallExit (Value s, Value s))
  nonLocalExitGet =
    coerce (callWithResult NonLocalExitGet)

  nonLocalExitSignal
    :: (WithCallStack, Foldable f)
    => Value s     -- ^ Error symbol
    -> f (Value s) -- ^ Error data, will be converted to a list as Emacs API expects.
    -> EmacsM s ()
  nonLocalExitSignal sym errData = do
    exception <- callWithResult $ NonLocalExitSignal (coerce sym) $
      foldMap (coerce (PtrBuilder.storable :: RawValue 'Regular -> PtrBuilder.Builder (RawValue 'Regular))) errData
    liftIO $ Exception.throwIO exception { emacsSignalOrigin = callStack }

  nonLocalExitThrow
    :: WithCallStack
    => Value s -- ^ Tag
    -> Value s -- ^ Data
    -> EmacsM s ()
  nonLocalExitThrow tag errData = do
    callNoResult $ NonLocalExitThrow (getRawValue tag) (getRawValue errData)
    liftIO $ Exception.throwIO EmacsThrow
      { emacsThrowTag    = tag'
      , emacsThrowValue  = errData'
      , emacsThrowOrigin = callStack
      }
    where
      tag'     = getRawValue tag
      errData' = getRawValue errData

  nonLocalExitClear :: WithCallStack => EmacsM s ()
  nonLocalExitClear = callNoResult NonLocalExitClear

  {-# INLINE makeFunction #-}
  makeFunction
    :: forall req opt rest s. (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => (forall s'. EmacsFunction req opt rest s' EmacsM)
    -> Doc.Doc
    -> EmacsM s (Value s)
  makeFunction emacsFun doc = do
    impl' <- liftIO $ exportToEmacs impl
    coerce $ callWithResult (MakeFunction (fromIntegral minArity) (fromIntegral maxArity) impl' doc)
    where
      (minArity, maxArity) = arities (Proxy @req) (Proxy @opt) (Proxy @rest)

      impl :: RawFunctionType 'Unknown ()
      impl env nargs argsPtr _extraPtr = do
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
  funcall name
    = coerce
    . callWithResultMayFailThrow
    . Funcall (getRawValue name)
    . foldMap (PtrBuilder.storable . getRawValue)

  {-# INLINE funcallPrimitive #-}
  funcallPrimitive
    :: (WithCallStack, Foldable f)
    => Value s
    -> f (Value s)
    -> EmacsM s (Value s)
  funcallPrimitive name
    = coerce
    . callWithResultMayFailThrow
    . FuncallPrimitive (getRawValue name)
    . foldMap (PtrBuilder.storable . getRawValue)

  {-# INLINE funcallPrimitiveUnchecked #-}
  funcallPrimitiveUnchecked
    :: (WithCallStack, Foldable f)
    => Value s
    -> f (Value s)
    -> EmacsM s (Value s)
  funcallPrimitiveUnchecked name
    = coerce
    . callWithResult
    . FuncallPrimitiveUnchecked (getRawValue name)
    . foldMap (PtrBuilder.storable . getRawValue)

  intern
    :: WithCallStack
    => SymbolName
    -> EmacsM s (Value s)
  intern
    = coerce
    . callWithResult
    . Intern

  typeOf
    :: WithCallStack
    => Value s -> EmacsM s (Value s)
  typeOf
    = coerce
    . callWithResult
    . TypeOf
    . getRawValue

  isNotNil :: WithCallStack => Value s -> EmacsM s Bool
  isNotNil = callWithResult . IsNotNil . getRawValue

  eq :: Value s -> Value s -> EmacsM s Bool
  eq x y =
    callWithResult (Eq (getRawValue x) (getRawValue y))

  extractWideInteger :: WithCallStack => Value s -> EmacsM s Int64
  extractWideInteger =
    callWithResultMayFailSignal . ExtractInteger . getRawValue

  makeWideInteger :: WithCallStack => Int64 -> EmacsM s (Value s)
  makeWideInteger =
    coerce . callWithResult . MakeInteger

  extractDouble :: WithCallStack => Value s -> EmacsM s Double
  extractDouble =
    callWithResultMayFailSignal . ExtractFloat . getRawValue

  makeDouble :: WithCallStack => Double -> EmacsM s (Value s)
  makeDouble =
    coerce . callWithResult . MakeFloat

  extractString :: WithCallStack => Value s -> EmacsM s BS.ByteString
  extractString =
    callWithResultMayFailSignal . ExtractString . getRawValue

  makeString :: WithCallStack => BS.ByteString -> EmacsM s (Value s)
  makeString =
    coerce . callWithResult . MakeString

  extractUserPtr :: WithCallStack => Value s -> EmacsM s (Ptr a)
  extractUserPtr =
    callWithResultMayFailSignal . GetUserPtr . getRawValue

  makeUserPtr
    :: WithCallStack
    => FinalizerPtr a
    -> Ptr a
    -> EmacsM s (Value s)
  makeUserPtr fin ptr =
    coerce $ callWithResult $ MakeUserPtr fin ptr

  assignUserPtr :: WithCallStack => Value s -> Ptr a -> EmacsM s ()
  assignUserPtr dest ptr =
    callWithResultMayFailSignal (SetUserPtr (getRawValue dest) ptr)

  extractUserPtrFinaliser
    :: WithCallStack => Value s -> EmacsM s (FinalizerPtr a)
  extractUserPtrFinaliser =
    callWithResultMayFailSignal . GetUserPtrFinaliser . getRawValue

  assignUserPtrFinaliser
    :: WithCallStack => Value s -> FinalizerPtr a -> EmacsM s ()
  assignUserPtrFinaliser x fin =
    callWithResultMayFailSignal (SetUserPtrFinaliser (getRawValue x) fin)

  vecGet :: WithCallStack => Value s -> Int -> EmacsM s (Value s)
  vecGet vec n
    = coerce
    $ callWithResultMayFailSignal (VecGet (getRawValue vec) n)

  vecSet
    :: WithCallStack
    => Value s -- ^ Vector
    -> Int     -- ^ Index
    -> Value s -- ^ New value
    -> EmacsM s ()
  vecSet vec n x
    = coerce
    $ callWithResultMayFailSignal (VecSet (getRawValue vec) n (getRawValue x))

  vecSize :: WithCallStack => Value s -> EmacsM s Int
  vecSize vec
    = coerce
    $ callWithResultMayFailSignal (VecSize (getRawValue vec))
