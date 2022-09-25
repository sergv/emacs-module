----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE MagicHash #-}

module Data.Emacs.Module.SymbolName
  ( SymbolName
  , SomeSymbolName(..)
  , mkSymbolName
  , mkSymbolNameUnsafe#
  , mkSymbolNameShortByteString

  , UseSymbolName(..)
  ) where

import Data.Emacs.Module.SymbolName.Internal

