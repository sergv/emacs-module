----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Args
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
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

-- | Type-level Peano numbers.
--
-- Indented to be used with @DataKinds@ extension enabled.
data Nat = Z | S Nat

class NatValue (n :: Nat) where
  natValue :: Proxy n -> Int

instance NatValue 'Z where
  {-# INLINE natValue #-}
  natValue _ = 0

instance forall n. NatValue n => NatValue ('S n) where
  {-# INLINE natValue #-}
  natValue _ = 1 + natValue (Proxy @n)

-- | Required argument of an exported function.
data R a b = R !a !b

-- | Optional argument of an exported function.
data O a b = O !(Maybe a) !b

-- | All other arguments of an exported function as a list.
newtype Rest a = Rest [a]

-- | End of argument list of an exported funciton.
data Stop a = Stop

-- | Specification of the arguments that exposed functions can receive from Emacs.
--
-- This type family allows to declaratively specify how many required and
-- optional arguments a function can take and whether it accepts rest arguments.
-- It's a direct translation of argument lists in Emacs lisp, e.g.
--
-- > (defun foo (x y z &optional w t &rest quux)
-- >   (+ (* x y z) (* (or w 1) (or t 2)) (length quux)))
--
-- The function above has 3 required arguments, 2 optional and also has
-- rest arguments. The type family below has two 'Nat's and one 'Bool'
-- to provide that info.
type family EmacsArgs (req :: Nat) (opt :: Nat) (rest :: Bool) (a :: Type) = (r :: Type) | r -> req opt rest a where
  EmacsArgs ('S n) opt    rest   a = R a (EmacsArgs n  opt rest a)
  EmacsArgs 'Z     ('S k) rest   a = O a (EmacsArgs 'Z k   rest a)
  EmacsArgs 'Z     'Z     'True  a = Rest a
  EmacsArgs 'Z     'Z     'False a = Stop a

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


-- | Helper to retrieve number of arguments a function takes for Emacs.
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
