----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.ProcessInput
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Emacs.Module.Env.ProcessInput
  ( ProcessInputResult(..)
  , processInputResultToNum
  , processInputResultFromNum
  ) where

import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter.Generics

#include <emacs-module.h>

data ProcessInputResult
  = ProcessInputContinue
  | ProcessInputQuit
  deriving (Eq, Ord, Show, Data, Generic, Lift)

{-# INLINE processInputResultToNum #-}
processInputResultToNum :: Num a => ProcessInputResult -> a
processInputResultToNum = \case
  ProcessInputContinue -> (#const emacs_process_input_continue)
  ProcessInputQuit     -> (#const emacs_process_input_quit)

{-# INLINE processInputResultFromNum #-}
processInputResultFromNum :: (Eq a, Num a) => a -> Maybe ProcessInputResult
processInputResultFromNum = \case
  (#const emacs_process_input_continue) -> Just ProcessInputContinue
  (#const emacs_process_input_quit)     -> Just ProcessInputQuit
  _                                     -> Nothing

