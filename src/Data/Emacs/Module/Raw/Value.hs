----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Value
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Emacs.Module.Raw.Value
  ( RawValue
  , unRawValue
  , Pinning(..)
  , toUnknown
  ) where

import Data.Emacs.Module.Raw.Value.Internal
