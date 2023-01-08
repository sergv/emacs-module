----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.GetRawValue
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

module Data.Emacs.Module.GetRawValue (GetRawValue(..)) where

import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.Value.Internal

class GetRawValue a where
  getRawValue :: a -> RawValue 'Regular

instance GetRawValue (RawValue 'Regular) where
  {-# INLINE getRawValue #-}
  getRawValue = id

instance GetRawValue (Value s) where
  {-# INLINE getRawValue #-}
  getRawValue = unValue
