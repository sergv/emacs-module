----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Emacs.Module.SymbolName
  ( SymbolName
  , mkSymbolName
  , mkSymbolNameShortByteString
  , useSymbolNameAsCString
  ) where

import Data.Emacs.Module.SymbolName.Internal

