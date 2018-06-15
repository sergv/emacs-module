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

module Data.Emacs.Module.Env.Internal
  ( Env(..)
  , toPtr
  , exportToEmacs
  , RawFunctionType
  , RawFunction(..)
  ) where

import Foreign
import Foreign.C.Types

import Data.Emacs.Module.NonNullPtr
import qualified Data.Emacs.Module.Value as Emacs

import Data.Emacs.Module.NonNullPtr.Internal

-- | Emacs environment, right from the 'emacs-module.h'.
newtype Env = Env { unEnv :: NonNullPtr Env }

{-# INLINE toPtr #-}
toPtr :: Env -> Ptr Env
toPtr = unNonNullPtr . unEnv


type RawFunctionType a =
     Env
  -> CPtrdiff           -- Number of arguments
  -> Ptr Emacs.RawValue -- Actual arguments
  -> Ptr a              -- Extra data
  -> IO Emacs.RawValue

foreign import ccall "wrapper"
  exportToEmacs :: RawFunctionType a -> IO (RawFunction a)

newtype RawFunction a = RawFunction { unRawFunction :: FunPtr (RawFunctionType a) }
