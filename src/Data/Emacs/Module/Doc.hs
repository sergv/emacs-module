----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Doc
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Defines type that provides function's documentation that would be visible
-- in Emacs.
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash           #-}

module Data.Emacs.Module.Doc
  ( Doc
  , mkLiteralDoc
  , mkByteStringDoc
  , useDocAsCString
  ) where

import Data.ByteString.Char8 qualified as C8
import Foreign.C.String
import GHC.Exts

data Doc
  = StaticDoc Addr#
  | DynamicDoc !C8.ByteString

-- | Indended to be used with unboxed string literals like this
--
-- @
-- mkLiteralDoc "foo"#
-- @
{-# INLINE mkLiteralDoc #-}
mkLiteralDoc :: Addr# -> Doc
mkLiteralDoc = StaticDoc

-- | Turn abritrary bytestring into 'Doc'.
{-# INLINE mkByteStringDoc #-}
mkByteStringDoc :: C8.ByteString -> Doc
mkByteStringDoc = DynamicDoc

{-# INLINE useDocAsCString #-}
useDocAsCString :: Doc -> (CString -> IO a) -> IO a
useDocAsCString doc f = case doc of
  StaticDoc addr -> f (Ptr addr)
  DynamicDoc str -> C8.useAsCString str f

