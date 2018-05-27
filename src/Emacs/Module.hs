----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Emacs.Module
  ( -- * Generic interface
    MonadEmacs(..)

    -- * Type synonyms
  , EmacsFunction
  , EmacsFunctionExtra
  , Raw.UserPtrFinaliserType
  , Raw.UserPtrFinaliser

    -- * Errors
  , EmacsError(..)
  , EmacsInternalError(..)
  , reportAllErrorsToEmacs

    -- * EmacsM
  , EmacsM
  , runEmacsM

    -- * Functions
  , bindFunction
  , makeFunction
  , extractInt
  , makeInt
  , extractText
  , makeText
  , extractVector
  , extractVectorWith

    -- * Reexports
  , MonadThrow
  , Throws
  ) where

import Control.Exception.Safe.Checked (MonadThrow, Throws)

import qualified Data.Emacs.Module.Env.Raw as Raw
import Emacs.Module.Errors
import Emacs.Module.Functions
import Emacs.Module.Monad
import Emacs.Module.Monad.Class
