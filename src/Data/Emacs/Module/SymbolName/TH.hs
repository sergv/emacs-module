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
import Data.Maybe
import Language.Haskell.TH
import System.IO.Unsafe

import Data.Emacs.Module.Raw.Env.Internal (Env)
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal qualified as Sym

cacheSym :: String -> Maybe String -> Q [Dec]
cacheSym sym bindingName = do
  ref      <- newName ("ref_" ++ binding)
  noinline <- pragInlD ref NoInline FunLike AllPhases
  refSig   <- sigD ref [t| IORef (Env -> IO (RawValue 'Pinned)) |]
  refDecl  <- valD (varP ref) (normalB [e| unsafePerformIO (Sym.mkSymbolNameCache $sym') |]) []
  symSig   <- sigD (mkName binding) [t| Sym.SymbolName |]
  symDecl  <- valD (varP (mkName binding)) (normalB [e| Sym.CachedSymbol $(varE ref) $sym' |]) []
  pure [noinline, refSig, refDecl, symSig, symDecl]
  where
    sym' :: ExpQ
    sym' = [e| Sym.mkSymbolNameString $(litE (stringL sym)) |]
    binding = fromMaybe sym bindingName
