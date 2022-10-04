{-# LANGUAGE DataKinds #-}

module Data.Emacs.Module.SymbolName.Internal
  ( SymbolName
  , mkSymbolNameCache
  , mkCachedSymbolName
  , mkSymbolNameString
  ) where

import Data.IORef

import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value

data SymbolName

mkSymbolNameCache :: SymbolName -> IO (IORef (Env -> IO (RawValue 'Pinned)))

mkCachedSymbolName :: IORef (Env -> IO (RawValue 'Pinned)) -> SymbolName -> SymbolName

mkSymbolNameString :: String -> SymbolName
