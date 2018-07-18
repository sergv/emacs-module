----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Args
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Emacs.Module.Args
  ( Nat(..)
  , EmacsArgs
  , EmacsInvocation(..)
  , GetArities(..)

    -- * Argument inference
  , R(..)
  , O(..)
  , Rest(..)
  , Stop(..)
  ) where

import Control.Monad.Base

import Data.Kind
import Data.Proxy
import Foreign
import Foreign.C.Types (CPtrdiff)

import Data.Emacs.Module.Raw.Env (variadicFunctionArgs)
import Data.Emacs.Module.Raw.Value

data Nat = Z | S Nat

class NatValue (n :: Nat) where
  natValue :: Proxy n -> Int

instance NatValue 'Z where
  {-# INLINE natValue #-}
  natValue _ = 0

instance forall n. NatValue n => NatValue ('S n) where
  {-# INLINE natValue #-}
  natValue _ = 1 + natValue (Proxy @n)

-- type family EmacsArgs' (req :: Nat) (m :: Type -> Type) (a :: Type) = (r :: Type) | r -> req m a where
--   EmacsArgs' ('S n) m a = RawValue       -> EmacsArgs' n  m a
--   -- EmacsArgs 'Z     m a = Maybe RawValue -> EmacsArgs' 'Z k   rest m a
--   -- EmacsArgs 'Z     m a = [RawValue]     -> m a
--   EmacsArgs' 'Z     m a = m a

-- type family EmacsArgs' (req :: Nat) (a :: Type) = (r :: Type) | r -> req
--
-- type instance EmacsArgs' ('S n) a = RawValue -> EmacsArgs' n a
--   -- EmacsArgs 'Z     m a = Maybe RawValue -> EmacsArgs' 'Z k   rest m a
--   -- EmacsArgs 'Z     m a = [RawValue]     -> m a
-- type instance EmacsArgs' 'Z     a = IO a

-- | Required argument.
data R a b = R !a !b

-- | Optional argument.
data O a b = O !(Maybe a) !b

-- | All other arguments as a list.
newtype Rest a = Rest [a]

-- | No more arguments.
data Stop a = Stop

type family EmacsArgs (req :: Nat) (opt :: Nat) (rest :: Bool) (a :: Type) = (r :: Type) | r -> req opt rest a where
  EmacsArgs ('S n) opt    rest   a = R a (EmacsArgs n  opt rest a)
  EmacsArgs 'Z     ('S k) rest   a = O a (EmacsArgs 'Z k   rest a)
  EmacsArgs 'Z     'Z     'True  a = Rest a
  EmacsArgs 'Z     'Z     'False a = Stop a

-- type family EmacsArgs (req :: Nat) (opt :: Nat) (rest :: Bool) (s :: Type) (a :: Type) = (r :: Type) | r -> req opt rest where
--   EmacsArgs ('S n) opt    rest   s a = Value s         -> EmacsArgs n  opt rest s a
--   EmacsArgs 'Z     ('S k) rest   s a = Maybe (Value s) -> EmacsArgs 'Z k   rest s a
--   EmacsArgs 'Z     'Z     'True  s a = [Value s]       -> IO a
--   EmacsArgs 'Z     'Z     'False s a = IO a

class EmacsInvocation req opt rest where
  supplyEmacsArgs
    :: MonadBase IO m
    => Int
    -> Ptr RawValue
    -> (RawValue -> m a)
    -> (EmacsArgs req opt rest a -> m b)
    -> m b

instance EmacsInvocation 'Z 'Z 'False where
  {-# INLINE supplyEmacsArgs #-}
  supplyEmacsArgs _nargs _startPtr _mkInput f = f Stop

instance EmacsInvocation 'Z 'Z 'True where
  {-# INLINE supplyEmacsArgs #-}
  supplyEmacsArgs
    :: MonadBase IO m
    => Int
    -> Ptr RawValue
    -> (RawValue -> m a)
    -> (Rest a -> m b)
    -> m b
  supplyEmacsArgs nargs startPtr mkArg f =
    case nargs of
      0 -> f (Rest [])
      n -> f . Rest =<< traverse mkArg =<< liftBase (peekArray n startPtr)

{-# INLINE advanceEmacsValuePtr #-}
advanceEmacsValuePtr :: Ptr RawValue -> Ptr RawValue
advanceEmacsValuePtr =
  (`plusPtr` (sizeOf (undefined :: RawValue)))

instance EmacsInvocation 'Z n rest => EmacsInvocation 'Z ('S n) rest where
  {-# INLINE supplyEmacsArgs #-}
  supplyEmacsArgs
    :: forall m a b. MonadBase IO m
    => Int
    -> Ptr RawValue
    -> (RawValue -> m a)
    -> (O a (EmacsArgs 'Z n rest a) -> m b)
    -> m b
  supplyEmacsArgs nargs startPtr mkArg f =
    case nargs of
      0 -> supplyEmacsArgs nargs startPtr mkArg (f . O Nothing)
      _ -> do
        arg <- mkArg =<< liftBase (peek startPtr)
        supplyEmacsArgs
          (nargs - 1)
          (advanceEmacsValuePtr startPtr)
          mkArg
          (f . O (Just arg))

instance EmacsInvocation n opt rest => EmacsInvocation ('S n) opt rest where
  {-# INLINE supplyEmacsArgs #-}
  supplyEmacsArgs
    :: MonadBase IO m
    => Int
    -> Ptr RawValue
    -> (RawValue -> m a)
    -> (R a (EmacsArgs n opt rest a) -> m b)
    -> m b
  supplyEmacsArgs nargs startPtr mkArg f = do
    arg <- mkArg =<< liftBase (peek startPtr)
    supplyEmacsArgs (nargs - 1) (advanceEmacsValuePtr startPtr) mkArg (f . R arg)


class GetArities (req :: Nat) (opt :: Nat) (rest :: Bool) where
  arities :: Proxy req -> Proxy opt -> Proxy rest -> (CPtrdiff, CPtrdiff)

instance (NatValue req, NatValue opt) => GetArities req opt 'False where
  {-# INLINE arities #-}
  arities preq popt _ = (req, req + opt)
    where
      req = fromIntegral (natValue preq)
      opt = fromIntegral (natValue popt)

instance NatValue req => GetArities req opt 'True where
  {-# INLINE arities #-}
  arities preq _ _ =
    (fromIntegral (natValue preq), variadicFunctionArgs)


-- data Args (req :: Nat) (opt :: Nat) (rest :: Bool) (a :: Type) where
--   NoArgs   ::                                   Args 'Z       'Z       'False a
--   ReqArg   :: a       -> Args req opt rest a -> Args ('S req) opt      rest   a
--   OptArg   :: Maybe a -> Args req opt rest a -> Args req      ('S opt) rest   a
--   RestArgs :: [a]                            -> Args 'Z       'Z       'True  a
--
-- deriving instance Functor     (Args req opt rest)
-- deriving instance Foldable    (Args req opt rest)
-- deriving instance Traversable (Args req opt rest)

-- class GetArgs (req :: Nat) (opt :: Nat) (rest :: Bool) where
--   getArgs :: Storable a => Int -> Ptr a -> IO (Args req opt rest a)
--
-- instance GetArgs 'Z 'Z 'False where
--   {-# INLINE getArgs #-}
--   getArgs !_nargs _startPtr = pure NoArgs
--
-- instance GetArgs 'Z 'Z 'True where
--   {-# INLINE getArgs #-}
--   getArgs !nargs startPtr =
--     case nargs of
--       0 -> pure $ RestArgs []
--       n -> RestArgs <$> peekArray n startPtr
--
-- instance (GetArgs req n rest, DefaultArgs req n rest) => GetArgs req ('S n) rest where
--   {-# INLINE getArgs #-}
--   getArgs :: forall a. Storable a => Int -> Ptr a -> IO (Args req ('S n) rest a)
--   getArgs !nargs startPtr = do
--     case nargs of
--       0 -> pure $ OptArg Nothing defaultArgs
--       _ -> OptArg <$> (Just <$> peek startPtr) <*> getArgs (nargs - 1) (plusPtr startPtr (sizeOf (undefined :: a)))
--
-- instance GetArgs n opt rest => GetArgs ('S n) opt rest where
--   {-# INLINE getArgs #-}
--   getArgs :: forall a. Storable a => Int -> Ptr a -> IO (Args ('S n) opt rest a)
--   getArgs !nargs startPtr = do
--     ReqArg <$> peek startPtr <*> getArgs (nargs - 1) (plusPtr startPtr (sizeOf (undefined :: a)))
--
--
-- class DefaultArgs (req :: Nat) (opt :: Nat) (rest :: Bool) where
--   defaultArgs :: Args req opt rest a
--
-- instance DefaultArgs 'Z 'Z 'False where
--   {-# INLINE defaultArgs #-}
--   defaultArgs = NoArgs
--
-- instance DefaultArgs 'Z 'Z 'True where
--   {-# INLINE defaultArgs #-}
--   defaultArgs = RestArgs []
--
-- instance DefaultArgs req n rest => DefaultArgs req ('S n) rest where
--   {-# INLINE defaultArgs #-}
--   defaultArgs = OptArg Nothing defaultArgs
