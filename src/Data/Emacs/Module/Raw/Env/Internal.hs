----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE UnliftedNewtypes         #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.Raw.Env.Internal
  ( Env(..)
  , Environment
  , toPtr
  , fromPtr
  , exportToEmacs
  , RawFunctionType
  , RawFunction(..)

  , freeHaskellFunPtrWrapped
  ) where

import Foreign
import Foreign.C.Types
import GHC.Exts (Addr#, Ptr(..))

import Data.Emacs.Module.Raw.Value.Internal

-- | Emacs environment, right from the 'emacs-module.h'.
newtype Env = Env { unEnv# :: Addr# }

data Environment

{-# INLINE toPtr #-}
toPtr :: Env -> Ptr Environment
toPtr (Env x) = Ptr x

{-# INLINE fromPtr #-}
fromPtr :: Ptr Environment -> Env
fromPtr (Ptr x) = Env x

type RawFunctionType o a
  =  Ptr Environment
  -> CPtrdiff                -- Number of arguments
  -> Ptr (RawValue 'Regular) -- Actual arguments, always supplied by Emacs so never 'Pinned'.
  -> Ptr a                   -- Extra data
  -> IO (RawValue o)

-- NB This is *the* coolest point of this library: *any* Haskell
-- function (incl closures) may be exposed to C to be called later.
-- The C/C++ will never have this...

-- | Take Haskell function and return C pointer to function (which
-- ideally needs to be cleaned up later by 'freeHaskellFunPtrWrapped').
foreign import ccall "wrapper"
  exportToEmacs :: RawFunctionType o a -> IO (RawFunction o a)

-- | Pointer to a function that may later be called by by Emacs.
newtype RawFunction o a = RawFunction { unRawFunction :: FunPtr (RawFunctionType o a) }
  deriving (Eq, Ord, Show)

-- This function is defined in base. See what 'freeHaskellFunPtr' for a start.
foreign import ccall unsafe "&freeHaskellFunctionPtr"
  freeHaskellFunPtrWrapped :: FinalizerPtr a
