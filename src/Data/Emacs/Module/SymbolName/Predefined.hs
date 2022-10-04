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

import Data.Emacs.Module.SymbolName.Predefined.Funcall

cacheSym "error"           Nothing
cacheSym "list"            Nothing
cacheSym "cons"            Nothing
cacheSym "car"             Nothing
cacheSym "cdr"             Nothing
cacheSym "setcar"          Nothing
cacheSym "setcdr"          Nothing
cacheSym "nil"             Nothing
cacheSym "fset"            Nothing
cacheSym "provide"         Nothing
cacheSym "t"               Nothing
cacheSym "vector"          Nothing
cacheSym "vconcat"         Nothing
cacheSym "face"            Nothing
cacheSym "propertize"      Nothing
cacheSym "concat"          Nothing
cacheSym "symbol-name"     (Just "symbolName")
cacheSym "prin1-to-string" (Just "prin1ToString")
