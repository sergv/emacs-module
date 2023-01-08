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

{-# LANGUAGE MagicHash #-}

module Data.Emacs.Module.Doc
  ( Doc
  , mkLiteralDoc
  , mkTextDoc
  , useDocAsCString
  ) where

import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Foreign.C.String
import GHC.Exts

data Doc
  = StaticDoc Addr#
  | DynamicDoc !Text

instance Show Doc where
  show = \case
    DynamicDoc x   -> show x
    StaticDoc addr -> show (unpackCString# addr)

instance IsString Doc where
  {-# INLINE fromString #-}
  fromString = mkStringDoc

mkStringDoc :: String -> Doc
mkStringDoc = mkTextDoc . T.pack

{-# INLINE [0] mkStringDoc #-}

{-# RULES
"Doc string literal" forall s .
   mkStringDoc (unpackCString# s) = mkLiteralDoc s
 #-}

-- | Indended to be used with unboxed string literals like this
--
-- @
-- mkLiteralDoc "foo"#
-- @
{-# INLINE mkLiteralDoc #-}
mkLiteralDoc :: Addr# -> Doc
mkLiteralDoc = StaticDoc

-- | Turn abritrary bytestring into 'Doc'.
{-# INLINE mkTextDoc #-}
mkTextDoc :: Text -> Doc
mkTextDoc = DynamicDoc

{-# INLINE useDocAsCString #-}
useDocAsCString :: Doc -> (CString -> IO a) -> IO a
useDocAsCString doc f = case doc of
  StaticDoc addr -> f (Ptr addr)
  DynamicDoc str -> T.withCString str f

