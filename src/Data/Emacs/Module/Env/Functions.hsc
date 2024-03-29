----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.Functions
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Emacs.Module.Env.Functions
  ( FuncallExit(..)
  , funcallExitToNum
  , funcallExitFromNum
  , foldFuncallExitFromNum
  ) where

import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter.Generics

#include <emacs-module.h>

-- | Possible Emacs function call outcomes. This is Haskell's version of
data FuncallExit a
  = -- | Function has returned normally.
    FuncallExitReturn
  | -- | Function has signaled an error using @signal@.
    FuncallExitSignal a
  | -- | Function has exit using @throw@.
    FuncallExitThrow a
  deriving (Eq, Ord, Show, Data, Generic, Lift, Functor, Foldable, Traversable)

instance Pretty a => Pretty (FuncallExit a) where
  pretty = ppGeneric

{-# INLINE funcallExitToNum #-}
funcallExitToNum :: Num a => FuncallExit b -> a
funcallExitToNum = \case
  FuncallExitReturn   -> (#const emacs_funcall_exit_return)
  FuncallExitSignal{} -> (#const emacs_funcall_exit_signal)
  FuncallExitThrow{}  -> (#const emacs_funcall_exit_throw)

{-# INLINE funcallExitFromNum #-}
funcallExitFromNum :: (Eq a, Num a) => a -> Maybe (FuncallExit ())
funcallExitFromNum = \case
  (#const emacs_funcall_exit_return) -> Just FuncallExitReturn
  (#const emacs_funcall_exit_signal) -> Just $ FuncallExitSignal ()
  (#const emacs_funcall_exit_throw)  -> Just $ FuncallExitThrow ()
  _                                  -> Nothing

{-# INLINE foldFuncallExitFromNum #-}
foldFuncallExitFromNum :: (Eq a, Num a) => a -> b -> (FuncallExit () -> b) -> b
foldFuncallExitFromNum x def f = case x of
  (#const emacs_funcall_exit_return) -> f FuncallExitReturn
  (#const emacs_funcall_exit_signal) -> f $ FuncallExitSignal ()
  (#const emacs_funcall_exit_throw)  -> f $ FuncallExitThrow ()
  _                                  -> def

