-- |
-- Module:     Data.Emacs.Module.SymbolName.Internal.Type
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DataKinds #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.SymbolName.Internal.Type (SymbolName(..)) where

import Data.IORef
import Data.Text (Text)
import Foreign.C.Types
import GHC.Ptr

import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value

data SymbolName
  = StaticSymbol  {-# UNPACK #-} !(Ptr CChar)
  | DynamicSymbol {-# UNPACK #-} !Text
  | CachedSymbol  (IORef (Env -> IO (RawValue 'Pinned))) SymbolName
  deriving (Eq)

