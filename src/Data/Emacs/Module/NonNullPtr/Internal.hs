----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.NonNullPtr.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Emacs.Module.NonNullPtr.Internal (NonNullPtr(..)) where

import Foreign

newtype NonNullPtr a = NonNullPtr { unNonNullPtr :: Ptr a }
  deriving (Eq, Ord, Show, Storable)

