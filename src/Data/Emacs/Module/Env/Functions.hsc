----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.Functions
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Emacs.Module.Env.Functions
  ( FuncallExit(..)
  , funcallExitToNum
  , funcallExitFromNum
  ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

#include <emacs-module.h>

-- | Possible Emacs function call outcomes. This is Haskell's version of
data FuncallExit a =
    -- | Function has returned normally.
    FuncallExitReturn
  | -- | Function has signaled an error using @signal@.
    FuncallExitSignal a
  | -- | Function has exit using @throw@.
    FuncallExitThrow a
  deriving (Eq, Ord, Show, Data, Generic, Lift, Typeable, Functor, Foldable, Traversable)

funcallExitToNum :: Num a => FuncallExit b -> a
funcallExitToNum = \case
  FuncallExitReturn   -> (#const emacs_funcall_exit_return)
  FuncallExitSignal{} -> (#const emacs_funcall_exit_signal)
  FuncallExitThrow{}  -> (#const emacs_funcall_exit_throw)

funcallExitFromNum :: (Eq a, Num a) => a -> Maybe (FuncallExit ())
funcallExitFromNum = \case
  (#const emacs_funcall_exit_return) -> Just FuncallExitReturn
  (#const emacs_funcall_exit_signal) -> Just $ FuncallExitSignal ()
  (#const emacs_funcall_exit_throw)  -> Just $ FuncallExitThrow ()
  _                                  -> Nothing

