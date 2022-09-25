----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.TH
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Emacs.Module.SymbolName.TH
  ( cacheSym
  ) where

import Data.IORef
import Language.Haskell.TH
import System.IO.Unsafe

import Data.Emacs.Module.Raw.Env.Internal (Env)
import Data.Emacs.Module.Raw.Value (GlobalRef)
import Data.Emacs.Module.SymbolName.Internal

cacheSym :: String -> Q [Dec]
cacheSym sym = do
  ref      <- newName ("ref_" ++ sym)
  noinline <- pragInlD ref NoInline FunLike AllPhases
  refSig   <- sigD ref [t| IORef (Env -> IO GlobalRef) |]
  refDecl  <- valD (varP ref) (normalB [e| unsafePerformIO (mkSymbolNameCache $sym') |]) []
  symSig   <- sigD (mkName sym) [t| SymbolName |]
  symDecl  <- valD (varP (mkName sym)) (normalB [e| CachedSymbol $(varE ref) $sym' |]) []
  pure [noinline, refSig, refDecl, symSig, symDecl]
  where
    sym' :: ExpQ
    sym' = [e| mkSymbolNameString $(litE (stringL sym)) |]
