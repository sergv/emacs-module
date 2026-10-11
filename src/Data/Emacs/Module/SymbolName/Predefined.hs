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
  , intern
  ) where

import Prelude hiding (error, concat)

import Data.Emacs.Module.SymbolName.TH

import Data.Emacs.Module.SymbolName.Internal.Type (SymbolName)
import Data.Emacs.Module.SymbolName.Predefined.Intern

cacheSym "error"           "errorTH"
cacheSym "list"            "listTH"
cacheSym "cons"            "consTH"
cacheSym "car"             "carTH"
cacheSym "cdr"             "cdrTH"
cacheSym "setcar"          "setcarTH"
cacheSym "setcdr"          "setcdrTH"
cacheSym "nil"             "nilTH"
cacheSym "fset"            "fsetTH"
cacheSym "provide"         "provideTH"
cacheSym "t"               "tTH"
cacheSym "vector"          "vectorTH"
cacheSym "vconcat"         "vconcatTH"
cacheSym "face"            "faceTH"
cacheSym "propertize"      "propertizeTH"
cacheSym "concat"          "concatTH"
cacheSym "symbol-name"     "symbolNameTH"
cacheSym "prin1-to-string" "prin1ToStringTH"
cacheSym "funcall"         "funcallTH"

{-# INLINE error #-}
error :: SymbolName
error = errorTH

{-# INLINE list #-}
list :: SymbolName
list = listTH

{-# INLINE cons #-}
cons :: SymbolName
cons = consTH

{-# INLINE car #-}
car :: SymbolName
car = carTH

{-# INLINE cdr #-}
cdr :: SymbolName
cdr = cdrTH

{-# INLINE setcar #-}
setcar :: SymbolName
setcar = setcarTH

{-# INLINE setcdr #-}
setcdr :: SymbolName
setcdr = setcdrTH

{-# INLINE nil #-}
nil :: SymbolName
nil = nilTH

{-# INLINE fset #-}
fset :: SymbolName
fset = fsetTH

{-# INLINE provide #-}
provide :: SymbolName
provide = provideTH

{-# INLINE t #-}
t :: SymbolName
t = tTH

{-# INLINE vector #-}
vector :: SymbolName
vector = vectorTH

{-# INLINE vconcat #-}
vconcat :: SymbolName
vconcat = vconcatTH

{-# INLINE face #-}
face :: SymbolName
face = faceTH

{-# INLINE propertize #-}
propertize :: SymbolName
propertize = propertizeTH

{-# INLINE concat #-}
concat :: SymbolName
concat = concatTH

{-# INLINE symbolName #-}
symbolName :: SymbolName
symbolName = symbolNameTH

{-# INLINE prin1ToString #-}
prin1ToString :: SymbolName
prin1ToString = prin1ToStringTH

{-# INLINE funcall #-}
funcall :: SymbolName
funcall = funcallTH

