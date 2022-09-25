----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.NonNullPtr
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Emacs.Module.NonNullPtr
  ( NonNullPtr
  , unNonNullPtr
  , mkNonNullPtr
  , allocaNonNull
  , allocaBytesNonNull
  ) where

import Data.Coerce
import Foreign

import Data.Emacs.Module.NonNullPtr.Internal
import Emacs.Module.Assert

mkNonNullPtr :: WithCallStack => Ptr a -> NonNullPtr a
#ifdef ASSERTIONS
mkNonNullPtr x
  | x == nullPtr = error "Assertion failed: trying to make non-null pointer from a null one"
  | otherwise    = NonNullPtr x
#else
mkNonNullPtr = NonNullPtr
#endif

{-# INLINE allocaNonNull #-}
allocaNonNull :: forall a b. Storable a => (NonNullPtr a -> IO b) -> IO b
allocaNonNull = coerce (alloca :: (Ptr a -> IO b) -> IO b)

{-# INLINE allocaBytesNonNull #-}
allocaBytesNonNull :: forall a b. Int -> (NonNullPtr a -> IO b) -> IO b
allocaBytesNonNull = coerce (allocaBytes :: Int -> (Ptr a -> IO b) -> IO b)
