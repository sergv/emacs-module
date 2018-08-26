----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.SymbolName.Internal
  ( SymbolName(..)
  , mkSymbolName
  , mkSymbolNameShortByteString
  , useSymbolNameAsCString
  ) where

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Unsafe as C8.Unsafe
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Text.Prettyprint.Doc
import Foreign.C.String

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
