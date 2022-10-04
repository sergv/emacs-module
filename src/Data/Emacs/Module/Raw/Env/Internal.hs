----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.Raw.Env.Internal
  ( Env(..)
  , toPtr
  , exportToEmacs
  , RawFunctionType
  , RawFunction(..)

  , freeHaskellFunPtrWrapped
  ) where

import Foreign
import Foreign.C.Types

import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Value.Internal

import Data.Emacs.Module.NonNullPtr.Internal

-- | Emacs environment, right from the 'emacs-module.h'.
newtype Env = Env { unEnv :: NonNullPtr Env }

{-# INLINE toPtr #-}
toPtr :: Env -> Ptr Env
toPtr = unNonNullPtr . unEnv

type RawFunctionType o a =
     Env
  -> CPtrdiff                -- Number of arguments
  -> Ptr (RawValue 'Regular) -- Actual arguments, always supplied by Emacs so never 'Pinned'.
  -> Ptr a                   -- Extra data
  -> IO (RawValue o)

-- NB This is *the* coolest point of this library: *any* Haskell
-- function (incl closures) may be exposed to C to be called later.
-- The C/C++ will never have this...
foreign import ccall "wrapper"
  exportToEmacs :: RawFunctionType o a -> IO (RawFunction o a)

newtype RawFunction o a = RawFunction { unRawFunction :: FunPtr (RawFunctionType o a) }
  deriving (Eq, Ord, Show)

-- This function is defined in base. See what 'freeHaskellFunPtr' for a start.
foreign import ccall unsafe "&freeHaskellFunctionPtr"
  freeHaskellFunPtrWrapped :: FinalizerPtr a
