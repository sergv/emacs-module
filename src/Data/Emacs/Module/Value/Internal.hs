----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Value.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Data.Emacs.Module.Value.Internal (Value(..)) where

import Control.DeepSeq
import Control.Monad.Trans.Resource

import GHC.Generics (Generic)

import Data.Emacs.Module.Raw.Value (GlobalRef(..))

-- | Value that is independent of environment ('Env') that produced it.
-- Incidentally, this implies that it's "protected" against Emacs GC and
-- thus will not unexpectedly go out of scope.
--
-- Can be used to e.g. cache values that are expensive to compute from scratch.
data Value s = Value
  { valuePayload       :: {-# UNPACK #-} !GlobalRef
  , valueReleaseHandle :: {-# UNPACK #-} !ReleaseKey
  } deriving (Generic)

instance NFData (Value s) where
  rnf (Value x y) = rnf x `seq` y `seq` ()
