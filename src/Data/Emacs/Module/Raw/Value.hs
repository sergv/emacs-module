----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Value
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Emacs.Module.Raw.Value (RawValue(..), GlobalRef(..)) where

import Control.DeepSeq

import Foreign

-- | Basic handle on an Emacs value. Can be GC'ed after any call into Emacs.
-- To overcome that, use 'ValueGC'.
--
-- Not a real pointer because emacs values are not really pointers. That is,
-- they're completely opaque.
newtype RawValue = RawValue { unRawValue :: Ptr RawValue }
  deriving (NFData, Storable)

-- | Value that is independent of environment ('Env') that produced it.
--
-- Can be used to e.g. cache values that are expensive to compute from scratch.
newtype GlobalRef = GlobalRef { unGlobalRef :: RawValue }
  deriving (NFData, Storable)
