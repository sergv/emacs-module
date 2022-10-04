----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Value.Internal
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Emacs.Module.Raw.Value.Internal
  ( RawValue(..)
  , Pinning(..)
  , toUnknown
  ) where

import Control.DeepSeq
import Data.Coerce
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Foreign
import GHC.Generics (Generic)
import Prettyprinter (Pretty(..))

data Pinning
  -- | Tag for global values are independent of environment ('Env') that produced it.
  --
  -- Can be used to e.g. cache values that are expensive to compute from scratch.
  = Pinned

  -- | Tag denoting regular Emacs values. Will go away after control
  -- returns to Emacs.
  | Regular

  -- | Tag denoting either global or regular emacs values. Cannot tell
  -- them apart, just pass to Emacs.
  | Unknown

toUnknown :: RawValue p -> RawValue 'Unknown
toUnknown = coerce

-- | Basic handle on an Emacs value.
newtype RawValue (p :: Pinning) = RawValue { unRawValue :: Ptr (RawValue p) }
  deriving (Show, NFData, Generic, Storable, Prim)

instance Pretty (RawValue p) where
  pretty = pretty . show . unRawValue

newtype instance U.MVector s (RawValue p) = MV_RawValue (P.MVector s (RawValue p))
newtype instance U.Vector    (RawValue p) = V_RawValue  (P.Vector    (RawValue p))

deriving via (U.UnboxViaPrim (RawValue p)) instance GM.MVector U.MVector (RawValue p)
deriving via (U.UnboxViaPrim (RawValue p)) instance G.Vector   U.Vector  (RawValue p)

instance U.Unbox (RawValue p)
