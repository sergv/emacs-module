{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Data.Emacs.Module.SymbolName.Internal
  ( SymbolName
  , mkSymbolNameCache
  , mkCachedSymbolName
  , mkSymbolNameString
  , mkSymbolNameUnsafe
  ) where

import Data.IORef

import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value
import GHC.Exts (Addr#)

import Data.Emacs.Module.SymbolName.Internal.Type

mkSymbolNameCache :: SymbolName -> IO (IORef (Env -> IO (RawValue 'Pinned)))

mkCachedSymbolName :: IORef (Env -> IO (RawValue 'Pinned)) -> SymbolName -> SymbolName

mkSymbolNameString :: String -> SymbolName

mkSymbolNameUnsafe :: Addr# -> SymbolName
