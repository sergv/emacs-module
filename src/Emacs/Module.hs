----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- This module is the entry point for writing Emacs extensions in
-- Haskell.
--
-- This package, though provides a lot of wrapping around Emacs's bare
-- C interface, still presumes some familiarity with said interface.
-- Thus, when developnig Emacs modules it's recommended to keep a
-- reference of the C interface around. One such reference is
-- <https://phst.github.io/emacs-modules.html>.
--
-- = Minimalistic example
--
-- Consider Emacs function
--
-- > (defun foo (f x y z &optional w t &rest quux)
-- >   (+ (funcall f (* x y z)) (* (or w 1) (or t 2)) (length quux)))
--
-- With help of this package, it may be defined as
--
-- @
-- {-# LANGUAGE DataKinds   #-}
-- {-# LANGUAGE QuasiQuotes #-}
--
-- import Data.Maybe
-- import Data.Emacs.Module.SymbolName.TH
-- import Emacs.Module
--
-- foo
--   :: (MonadEmacs m, Monad (m s))
--   => EmacsFunction ('S ('S ('S ('S 'Z)))) ('S ('S 'Z)) 'True s m
-- foo (R f (R x (R y (R z (O w (O t (Rest quux))))))) = do
--   x'    <- extractInt x
--   y'    <- extractInt y
--   z'    <- extractInt z
--   w'    <- traverse extractInt w
--   t'    <- traverse extractInt t
--
--   tmp   <- makeInt (x' * y' * z')
--   tmp'  <- extractInt =<< funcall [esym|funcall|] [f, tmp]
--
--   produceRef =<< makeInt (tmp' + fromMaybe 1 w' * fromMaybe 2 t' + length quux)
-- @
--
-- = Creating Emacs dynamic module
-- In order to make shared object or dll callable from Emacs,
-- a cabal project with foreign-library section has to be created.
-- Please refer to <https://github.com/sergv/emacs-module/tree/master/test>
-- for such a project.
--
-- Please note that this project will need a small C file for initialising
-- Haskell runtime. In the project mentioned before it's present as
-- <https://github.com/sergv/emacs-module/blob/master/test/cbits/emacs_wrapper.c>
----------------------------------------------------------------------------

module Emacs.Module
  (
    -- * EmacsM
    EmacsM
  , runEmacsM

    -- * Basic bindings
  , MonadEmacs(..)

    -- ** Define functions callable by Emacs
  , EmacsFunction
  , EmacsFunctionExtra
  , Nat(..)
  , R(..)
  , O(..)
  , Rest(..)
  , Stop(..)

    -- ** Error types
  , EmacsError(..)
  , EmacsInternalError(..)
  , reportAllErrorsToEmacs

    -- ** Other types
  , Raw.UserPtrFinaliserType
  , Raw.UserPtrFinaliser

    -- * Reexports
  , module Emacs.Module.Functions
  , module Data.Emacs.Module.Value
  , Env

    -- * Third-party reexports
  , MonadThrow
  , Throws
  ) where

import Control.Exception.Safe.Checked (MonadThrow, Throws)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Env (Env)
import qualified Data.Emacs.Module.Raw.Env as Raw
import Data.Emacs.Module.Value
import Emacs.Module.Errors
import Emacs.Module.Functions
import Emacs.Module.Monad
import Emacs.Module.Monad.Class
