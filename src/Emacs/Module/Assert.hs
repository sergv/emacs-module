----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Assert
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}

module Emacs.Module.Assert
  ( WithCallStack
  , emacsAssert
  ) where

import Data.Kind (Constraint)

#ifdef ASSERTIONS
import GHC.Stack (HasCallStack)
#endif

-- | Call stacks for all emacs-related functions in Haskell.
-- Will be disabled unless this package was build with 'assertions'
-- flag enabled.
#ifdef ASSERTIONS
type WithCallStack = (HasCallStack :: Constraint)
#else
type WithCallStack = (() :: Constraint)
#endif

#ifdef ASSERTIONS
emacsAssert :: WithCallStack => Bool -> String -> a -> a
emacsAssert True  _   = id
emacsAssert False msg = error $ "Assertion failed: " ++ msg
#else
{-# INLINE emacsAssert #-}
emacsAssert :: WithCallStack => Bool -> String -> a -> a
emacsAssert _ _ = id
#endif
