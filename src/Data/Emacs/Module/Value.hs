----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.RawValue
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Emacs.Module.Value
  ( RawValue(..)
  , GlobalRef(..)
  ) where

import Data.Emacs.Module.Raw.Value
