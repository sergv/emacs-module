----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.ProcessInput
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Emacs.Module.Env.ProcessInput
  ( Result(..)
  , resultToNum
  , resultFromNum
  ) where

import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter.Generics

#include <emacs-module.h>

-- | Result of 'process_inputs' Emacs API call.
data Result
  = Continue
  | Quit
  deriving (Eq, Ord, Show, Data, Generic, Lift)

instance Pretty Result where
  pretty = ppGeneric

{-# INLINE resultToNum #-}
resultToNum :: Num a => Result -> a
resultToNum = \case
  Continue -> (#const emacs_process_input_continue)
  Quit     -> (#const emacs_process_input_quit)

{-# INLINE resultFromNum #-}
resultFromNum :: (Eq a, Num a) => a -> Maybe Result
resultFromNum = \case
  (#const emacs_process_input_continue) -> Just Continue
  (#const emacs_process_input_quit)     -> Just Quit
  _                                     -> Nothing

