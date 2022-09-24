----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.ValueStore
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Emacs.Module.ValueStore
  ( ReleaseHandle
  , ValueStore
  , dummyReleaseHandle
  , new
  , add
  , forget
  , traverse_
  ) where

import Data.Emacs.Module.ValueStore.Internal
