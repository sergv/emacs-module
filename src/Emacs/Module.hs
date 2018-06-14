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

    -- * Reexports
  , module Emacs.Module.Functions
  , Env

    -- * Third-party reexports
  , MonadThrow
  , Throws
  ) where

import Control.Exception.Safe.Checked (MonadThrow, Throws)

import Data.Emacs.Module.Env (Env)
import qualified Data.Emacs.Module.Env.Raw as Raw
import Emacs.Module.Errors
import Emacs.Module.Functions
import Emacs.Module.Monad
import Emacs.Module.Monad.Class
