----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.TH
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Data.Emacs.Module.SymbolName.TH (esym) where

import qualified Data.ByteString.Char8 as C8
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Emacs.Module.SymbolName.Internal

-- | Quasi-quoter for 'SymbolName'. Avoids some runtime overhead of
-- creating a 'SymbolName', but in other respects is absolutely equivalent
-- to 'mkSymbolName'.
--
-- > [esym|foo|] == mkSymbolName "foo"
-- True
esym :: QuasiQuoter
esym = QuasiQuoter
  { quoteExp  = mkESym
  , quotePat  = const $ fail "Only defined for values"
  , quoteType = const $ fail "Only defined for values"
  , quoteDec  = const $ fail "Only defined for values"
  }

mkESym :: String -> ExpQ
mkESym s = [e| SymbolName (C8.pack $(stringE $ s ++ "\0")) |]
