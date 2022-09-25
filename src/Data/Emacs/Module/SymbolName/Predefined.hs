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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

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

import Data.Emacs.Module.SymbolName.Internal

{-# NOINLINE error #-}
error :: SymbolName (Cached "error")
error = cacheSymbolName (mkSymbolNameUnsafe# "error"#)

{-# NOINLINE list #-}
list :: SymbolName (Cached "list")
list = cacheSymbolName (mkSymbolNameUnsafe# "list"#)

{-# NOINLINE cons #-}
cons :: SymbolName (Cached "cons")
cons = cacheSymbolName (mkSymbolNameUnsafe# "cons"#)

{-# NOINLINE car #-}
car :: SymbolName (Cached "car")
car = cacheSymbolName (mkSymbolNameUnsafe# "car"#)

{-# NOINLINE cdr #-}
cdr :: SymbolName (Cached "cdr")
cdr = cacheSymbolName (mkSymbolNameUnsafe# "cdr"#)

{-# NOINLINE setcar #-}
setcar :: SymbolName (Cached "setcar")
setcar = cacheSymbolName (mkSymbolNameUnsafe# "setcar"#)

{-# NOINLINE setcdr #-}
setcdr :: SymbolName (Cached "setcdr")
setcdr = cacheSymbolName (mkSymbolNameUnsafe# "setcdr"#)

{-# NOINLINE nil #-}
nil :: SymbolName (Cached "nil")
nil = cacheSymbolName (mkSymbolNameUnsafe# "nil"#)

{-# NOINLINE fset #-}
fset :: SymbolName (Cached "fset")
fset = cacheSymbolName (mkSymbolNameUnsafe# "fset"#)

{-# NOINLINE provide #-}
provide :: SymbolName (Cached "provide")
provide = cacheSymbolName (mkSymbolNameUnsafe# "provide"#)

{-# NOINLINE t #-}
t :: SymbolName (Cached "t")
t = cacheSymbolName (mkSymbolNameUnsafe# "t"#)

{-# NOINLINE vector #-}
vector :: SymbolName (Cached "vector")
vector = cacheSymbolName (mkSymbolNameUnsafe# "vector"#)

{-# NOINLINE vconcat #-}
vconcat :: SymbolName (Cached "vconcat")
vconcat = cacheSymbolName (mkSymbolNameUnsafe# "vconcat"#)

{-# NOINLINE face #-}
face :: SymbolName (Cached "face")
face = cacheSymbolName (mkSymbolNameUnsafe# "face"#)

{-# NOINLINE propertize #-}
propertize :: SymbolName (Cached "propertize")
propertize = cacheSymbolName (mkSymbolNameUnsafe# "propertize"#)

{-# NOINLINE concat #-}
concat :: SymbolName (Cached "concat")
concat = cacheSymbolName (mkSymbolNameUnsafe# "concat"#)

{-# NOINLINE prin1ToString #-}
prin1ToString :: SymbolName (Cached "prin1-to-string")
prin1ToString = cacheSymbolName (mkSymbolNameUnsafe# "prin1-to-string"#)

{-# NOINLINE symbolName #-}
symbolName :: SymbolName (Cached "symbol-name")
symbolName = cacheSymbolName (mkSymbolNameUnsafe# "symbol-name"#)

{-# NOINLINE funcall #-}
funcall :: SymbolName (Cached "funcall")
funcall = cacheSymbolName (mkSymbolNameUnsafe# "funcall"#)
