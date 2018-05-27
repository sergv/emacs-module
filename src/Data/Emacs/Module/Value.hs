----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Value
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Emacs.Module.Value
  ( Value(..)
  , GlobalRef(..)
  ) where

import Foreign

-- | Not a real pointer because emacs values are not really pointers. That is,
-- they're completely opaque.
newtype Value = Value { unValue :: Ptr Value }
  deriving (Storable)

-- | Value that is independent of environment ('Env') that produced it.
--
-- Can be used to e.g. cache values that are expensive to compute from scratch.
newtype GlobalRef = GlobalRef { unGlobalRef :: Value }
  deriving (Storable)
