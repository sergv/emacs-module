----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Assert
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
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

#ifdef CALL_STACKS
import GHC.Stack (HasCallStack)
#endif

-- | Call stacks for all emacs-related functions in Haskell.
-- Will be disabled unless this package was build with 'call-stacks'
-- flag enabled.
#ifdef CALL_STACKS
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
emacsAssert :: Bool -> String -> a -> a
emacsAssert _ _ = id
#endif
