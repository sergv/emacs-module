----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Value
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Emacs.Module.Raw.Value
  ( RawValue(..)
  , GlobalRef(..)
  , dummyGlobalRef
  ) where

import Control.DeepSeq
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Foreign

-- | Basic handle on an Emacs value. Can be GC'ed after any call into Emacs.
-- To overcome that, use 'ValueGC'.
--
-- Not a real pointer because emacs values are not really pointers. That is,
-- they're completely opaque.
newtype RawValue = RawValue { unRawValue :: Ptr RawValue }
  deriving (NFData, Storable, Prim)

-- | Value that is independent of environment ('Env') that produced it.
--
-- Can be used to e.g. cache values that are expensive to compute from scratch.
newtype GlobalRef = GlobalRef { unGlobalRef :: RawValue }
  deriving (NFData, Storable, Prim)

dummyGlobalRef :: GlobalRef
dummyGlobalRef = GlobalRef (RawValue nullPtr)

newtype instance U.MVector s RawValue = MV_RawValue (P.MVector s RawValue)
newtype instance U.Vector    RawValue = V_RawValue  (P.Vector    RawValue)

deriving via (U.UnboxViaPrim RawValue) instance GM.MVector U.MVector RawValue
deriving via (U.UnboxViaPrim RawValue) instance G.Vector   U.Vector  RawValue

instance U.Unbox RawValue

newtype instance U.MVector s GlobalRef = MV_GlobalRef (P.MVector s GlobalRef)
newtype instance U.Vector    GlobalRef = V_GlobalRef  (P.Vector    GlobalRef)

deriving via (U.UnboxViaPrim GlobalRef) instance GM.MVector U.MVector GlobalRef
deriving via (U.UnboxViaPrim GlobalRef) instance G.Vector   U.Vector  GlobalRef

instance U.Unbox GlobalRef


