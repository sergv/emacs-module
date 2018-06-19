----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.Raw.Env.Internal
  ( Env(..)
  , toPtr
  , exportToEmacs
  , RawFunctionType
  , RawFunction(..)
  ) where

import Foreign
import Foreign.C.Types

import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Value

import Data.Emacs.Module.NonNullPtr.Internal

-- | Emacs environment, right from the 'emacs-module.h'.
newtype Env = Env { unEnv :: NonNullPtr Env }

{-# INLINE toPtr #-}
toPtr :: Env -> Ptr Env
toPtr = unNonNullPtr . unEnv


type RawFunctionType a =
     Env
  -> CPtrdiff     -- Number of arguments
  -> Ptr RawValue -- Actual arguments
  -> Ptr a        -- Extra data
  -> IO RawValue

-- NB This is *the* coolest point of this library: *any* Haskell
-- function (incl closures) may be exposed to C to be called later.
-- The C/C++ will never have this...
foreign import ccall "wrapper"
  exportToEmacs :: RawFunctionType a -> IO (RawFunction a)

newtype RawFunction a = RawFunction { unRawFunction :: FunPtr (RawFunctionType a) }
