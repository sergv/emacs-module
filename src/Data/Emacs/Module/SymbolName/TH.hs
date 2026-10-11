----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.TH
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Emacs.Module.SymbolName.TH
  ( cacheSym
  ) where

import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Language.Haskell.TH
import System.IO.Unsafe

import Data.Emacs.Module.Raw.Env.Internal (Env)
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal qualified as Sym

cacheSym :: String -> String -> Q [Dec]
cacheSym sym bindingName = do
  ref      <- newName ("ref_" ++ bindingName)
  noinline <- pragInlD ref NoInline FunLike AllPhases
  refSig   <- sigD ref [t| IORef (Env -> IO (RawValue 'Pinned)) |]
  refDecl  <- valD (varP ref) (normalB [e| unsafePerformIO (Sym.mkSymbolNameCache $sym') |]) []
  symSig   <- sigD (mkName bindingName) [t| Sym.SymbolName |]
  symDecl  <- valD (varP (mkName bindingName)) (normalB [e| Sym.CachedSymbol $(varE ref) $sym' |]) []
  pure [noinline, refSig, refDecl, symSig, symDecl]
  where
    sym' :: ExpQ
    sym' = [e| Sym.mkSymbolNameUnsafe $(litE (StringPrimL (BS.unpack (T.encodeUtf8 (T.pack sym))))) |]
