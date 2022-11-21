----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Runtime
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes    #-}

module Data.Emacs.Module.Runtime
  ( Runtime(..)
  , validateRuntime
  , withEnvironment
  ) where

import Control.Monad.Base

import Foreign
import Foreign.C.Types

import Data.Emacs.Module.Env qualified as Emacs
import Data.Emacs.Module.Raw.Env.Internal (Env(..))
import Data.Emacs.Module.NonNullPtr

import Data.Emacs.Module.NonNullPtr.Internal

#include <emacs-module.h>

-- | Emacs environment, right from the 'emacs-module.h'.
newtype Runtime = Runtime { unRuntime :: NonNullPtr Runtime }

type GetEnvironentType = Runtime -> Emacs.Env

foreign import ccall unsafe "dynamic" emacs_get_environment
  :: FunPtr GetEnvironentType -> GetEnvironentType

validateRuntime :: MonadBase IO m => Ptr Runtime -> m (Maybe Runtime)
validateRuntime !ptr
  | ptr == nullPtr = pure Nothing
  | otherwise      = liftBase $ do
      size <- (#peek struct emacs_runtime, size) ptr
      pure $ if expectedSize <= size then Just (Runtime (NonNullPtr ptr)) else Nothing
  where
    expectedSize :: CPtrdiff
    expectedSize = (#size struct emacs_runtime)

withEnvironment :: Runtime -> (Emacs.Env -> IO a) -> IO a
withEnvironment !runtime k = do
  (funPtr :: FunPtr GetEnvironentType) <- (#peek struct emacs_runtime, get_environment) (unNonNullPtr $ unRuntime runtime)
  k (emacs_get_environment funPtr runtime)
