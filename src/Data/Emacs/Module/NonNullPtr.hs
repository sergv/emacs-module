----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.NonNullPtr
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Data.Emacs.Module.NonNullPtr
  ( NonNullPtr
  , unNonNullPtr
  , mkNonNullPtr
  , allocaNonNull
  , allocaBytesNonNull
  ) where

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
allocaNonNull :: Storable a => (NonNullPtr a -> IO b) -> IO b
allocaNonNull f = alloca (f . NonNullPtr)

{-# INLINE allocaBytesNonNull #-}
allocaBytesNonNull :: Storable a => Int -> (NonNullPtr a -> IO b) -> IO b
allocaBytesNonNull n f = allocaBytes n (f . NonNullPtr)
