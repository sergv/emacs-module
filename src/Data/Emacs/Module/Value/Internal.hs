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
-- In order to prevent memory leaks, value is registered in the Emacs
-- monad than produced it and will be freed when the monad finishes.
-- To make the connection clear the value is tagged with parameter
-- @s@, which serves the same purpose as tag of the 'ST' monad. That
-- is, it ensures that value cannot leave the scope of the monad that
-- produced it.
data Value s = Value
  { valuePayload       :: {-# UNPACK #-} !GlobalRef
  , valueReleaseHandle :: {-# UNPACK #-} !ReleaseKey
  } deriving (Generic)

instance NFData (Value s) where
  rnf (Value x y) = rnf x `seq` y `seq` ()
