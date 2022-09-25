----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Predefined
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Some commonly used symbol names which will get initialized only
-- once after they're used.
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Emacs.Module.SymbolName.Predefined
  ( error
  , list
  , cons
  , car
  , cdr
  , setcar
  , setcdr
  , nil
  , fset
  , provide
  , t
  , vector
  , vconcat
  , face
  , propertize
  , concat
  , symbolName
  , prin1ToString
  , funcall
  ) where

import Prelude hiding (error, concat)

import Data.Emacs.Module.SymbolName.TH

cacheSym "error"
cacheSym "list"
cacheSym "cons"
cacheSym "car"
cacheSym "cdr"
cacheSym "setcar"
cacheSym "setcdr"
cacheSym "nil"
cacheSym "fset"
cacheSym "provide"
cacheSym "t"
cacheSym "vector"
cacheSym "vconcat"
cacheSym "face"
cacheSym "propertize"
cacheSym "concat"
cacheSym "symbolName"
cacheSym "prin1ToString"
cacheSym "funcall"
