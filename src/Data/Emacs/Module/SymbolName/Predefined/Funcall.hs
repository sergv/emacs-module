----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Predefined.Funcall
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

module Data.Emacs.Module.SymbolName.Predefined.Funcall
  ( funcall
  ) where

import Data.IORef
import System.IO.Unsafe

import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value

import {-# SOURCE #-} Data.Emacs.Module.SymbolName.Internal

funcallSym :: SymbolName
funcallSym = mkSymbolNameString "funcall"

{-# NOINLINE funcallCache #-}
funcallCache :: IORef (Env -> IO (RawValue 'Pinned))
funcallCache = unsafePerformIO $ mkSymbolNameCache funcallSym

funcall :: SymbolName
funcall = mkCachedSymbolName funcallCache funcallSym

