----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Value.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE UnboxedTuples #-}

module Data.Emacs.Module.Value.Internal
  ( Value(..)
  ) where

import Control.DeepSeq
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import GHC.Generics (Generic)
import Prettyprinter (Pretty)

import Data.Emacs.Module.Raw.Value

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
newtype Value (s :: k) = Value
  { unValue :: RawValue 'Regular
  } deriving (Show, NFData, Generic, Prim, Pretty)

newtype instance U.MVector s (Value s') = MV_Value (P.MVector s (Value s'))
newtype instance U.Vector    (Value s') = V_Value  (P.Vector    (Value s'))

deriving via (U.UnboxViaPrim (Value s')) instance GM.MVector U.MVector (Value s')
deriving via (U.UnboxViaPrim (Value s')) instance G.Vector   U.Vector  (Value s')

instance U.Unbox (Value s')

