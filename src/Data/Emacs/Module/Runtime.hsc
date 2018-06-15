----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Runtime
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Emacs.Module.Runtime
  ( Runtime(..)
  , validateRuntime
  , getEnvironment
  ) where

import Control.Monad.Base

import Foreign
import Foreign.C.Types

import qualified Data.Emacs.Module.Env as Emacs
import Data.Emacs.Module.Raw.Env.Internal (Env(..))
import Data.Emacs.Module.NonNullPtr

import Data.Emacs.Module.NonNullPtr.Internal

#include <emacs-module.h>

-- | Emacs environment, right from the 'emacs-module.h'.
newtype Runtime = Runtime { unRuntime :: NonNullPtr Runtime }

type GetEnvironentType = Runtime -> IO Emacs.Env

foreign import ccall unsafe "dynamic" emacs_get_environment
  :: FunPtr GetEnvironentType -> GetEnvironentType

validateRuntime :: MonadBase IO m => Ptr Runtime -> m (Maybe Runtime)
validateRuntime ptr
  | ptr == nullPtr = pure Nothing
  | otherwise      = liftBase $ do
      size <- (#peek struct emacs_runtime, size) ptr
      pure $ if expectedSize <= size then Just (Runtime (NonNullPtr ptr)) else Nothing
  where
    expectedSize :: CPtrdiff
    expectedSize = (#size struct emacs_runtime)

getEnvironment :: MonadBase IO m => Runtime -> m Emacs.Env
getEnvironment runtime = liftBase $ do
  (funPtr :: FunPtr GetEnvironentType) <- (#peek struct emacs_runtime, get_environment) (unNonNullPtr $ unRuntime runtime)
  emacs_get_environment funPtr runtime
