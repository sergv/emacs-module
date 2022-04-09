----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.SymbolName.Internal
  ( SymbolName(..)
  , mkSymbolName
  , mkSymbolNameShortByteString
  , useSymbolNameAsCString
  ) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Short qualified as BSS
import Data.ByteString.Unsafe qualified as C8.Unsafe
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Foreign.C.String
import Prettyprinter

newtype SymbolName = SymbolName { unSymbolName :: C8.ByteString }
  deriving (Eq, Ord, Show)

instance Pretty SymbolName where
  pretty = pretty . TE.decodeUtf8With TE.lenientDecode . C8.init . unSymbolName

{-# INLINE mkSymbolName #-}
mkSymbolName :: C8.ByteString -> SymbolName
mkSymbolName = SymbolName . (`C8.snoc` '\0')

{-# INLINE mkSymbolNameShortByteString #-}
mkSymbolNameShortByteString :: BSS.ShortByteString -> SymbolName
mkSymbolNameShortByteString = mkSymbolName . BSS.fromShort

{-# INLINE useSymbolNameAsCString #-}
useSymbolNameAsCString :: SymbolName -> (CString -> IO a) -> IO a
useSymbolNameAsCString = C8.Unsafe.unsafeUseAsCString . unSymbolName
