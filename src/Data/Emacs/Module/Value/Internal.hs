----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Value.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Emacs.Module.Value.Internal
  ( Value(..)
  ) where

import Control.DeepSeq
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import GHC.Generics (Generic)

import Data.Emacs.Module.Raw.Value (GlobalRef(..))
import Data.Emacs.Module.ValueStore.Internal (ReleaseHandle(..))

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
data Value (s :: k) = Value
  { valuePayload       :: {-# UNPACK #-} !GlobalRef
  , valueReleaseHandle :: {-# UNPACK #-} !ReleaseHandle
  } deriving (Generic)

instance NFData (Value s)

instance U.IsoUnbox (Value a) (GlobalRef, ReleaseHandle)

newtype instance U.MVector s (Value _) = MV_Value (U.MVector s (GlobalRef, ReleaseHandle))
newtype instance U.Vector    (Value _) = V_Value  (U.Vector    (GlobalRef, ReleaseHandle))

deriving via (Value s' `U.As` (GlobalRef, ReleaseHandle)) instance GM.MVector U.MVector (Value s')
deriving via (Value s' `U.As` (GlobalRef, ReleaseHandle)) instance G.Vector   U.Vector  (Value s')

instance U.Unbox (Value s')
