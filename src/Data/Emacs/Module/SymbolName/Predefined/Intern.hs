----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Predefined.Intern
-- Copyright   :  (c) Sergey Vinokurov 2026
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Data.Emacs.Module.SymbolName.Predefined.Intern
  ( intern
  ) where

import Data.IORef
import System.IO.Unsafe

import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value

import {-# SOURCE #-} Data.Emacs.Module.SymbolName.Internal

internSym :: SymbolName
internSym = mkSymbolNameUnsafe "intern"#

{-# NOINLINE internCache #-}
internCache :: IORef (Env -> IO (RawValue 'Pinned))
internCache = unsafePerformIO $ mkSymbolNameCache internSym

intern :: SymbolName
intern = mkCachedSymbolName internCache internSym

