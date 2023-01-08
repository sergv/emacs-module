----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.NonNullPtr.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Emacs.Module.NonNullPtr.Internal (NonNullPtr(..)) where

import Control.DeepSeq
import Foreign

newtype NonNullPtr a = NonNullPtr { unNonNullPtr :: Ptr a }
  deriving (Eq, Ord, Show, NFData, Storable)

