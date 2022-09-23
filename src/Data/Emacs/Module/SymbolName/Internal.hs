----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnliftedNewtypes           #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.SymbolName.Internal
  ( Static(..)
  , Dynamic(..)
  , UseSymbolName(..)
  , SymbolName(..)
  , SomeSymbolName(..)
  , EmacsSymbolName
  , mkSymbolName
  , mkSymbolNameUnsafe#
  , mkSymbolNameShortByteString
  ) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Kind
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Foreign.C.String
import Foreign.C.Types
import GHC.Exts (Addr#, byteArrayContents#)
import GHC.Ptr
import Prettyprinter

-- | Symbols that are known at compile time.
--
-- Will just pass pointer to 0-terminated statically-allocated string
-- to Emacs API when used.
newtype Static = Static (Ptr CChar)

newtype Dynamic = Dynamic { unDynamic :: BSS.ShortByteString }
  deriving (Eq, Ord, Show)

newtype SymbolName a = SymbolName a
  deriving (Eq, Ord, Show, Pretty)

instance Pretty Static where
  pretty (Static (Ptr addr))
    = pretty
    $ TE.decodeUtf8With TE.lenientDecode
    $ BS.unsafePackLiteral addr

instance Pretty Dynamic where
  pretty (Dynamic str)
    = pretty
    $ TE.decodeUtf8With TE.lenientDecode
    $ C8.init
    $ BSS.fromShort str

class UseSymbolName a where
  withSymbolNameAsCString :: SymbolName a -> (CString -> IO b) -> IO b

instance UseSymbolName Static where
  {-# INLINE withSymbolNameAsCString #-}
  withSymbolNameAsCString (SymbolName (Static addr)) f = f addr

instance UseSymbolName Dynamic where
  -- I like to live dangerously...
  {-# INLINE withSymbolNameAsCString #-}
  withSymbolNameAsCString (SymbolName (Dynamic (BSS.SBS arr))) f = f (Ptr (byteArrayContents# arr))

{-# INLINE mkSymbolName #-}
mkSymbolName :: C8.ByteString -> SymbolName Dynamic
mkSymbolName = mkSymbolNameShortByteString . BSS.toShort

data SomeSymbolName (c :: Type -> Constraint) = forall a. c a => SomeSymbolName (SymbolName a)

-- | Just aggregates various instances that various SymbolNames (e.g.
-- bouth dynamic and static) can be expected to satisfy.
class (Pretty a, UseSymbolName a) => EmacsSymbolName a
instance (Pretty a, UseSymbolName a) => EmacsSymbolName a

-- | Should be applied to unboxed string literals like this
--
-- @
-- mkSymbolNameUnsafe# "foo"#
-- @
--
-- Can be safely applied to non-literals (e.g. arbitrary pointers) if
-- it's guaranteed that address points to a null-terminated strings.
-- Otherwise behaviour is undefined.
{-# INLINE mkSymbolNameUnsafe# #-}
mkSymbolNameUnsafe# :: Addr# -> SymbolName Static
mkSymbolNameUnsafe# addr = SymbolName (Static (Ptr addr))

{-# INLINE mkSymbolNameShortByteString #-}
mkSymbolNameShortByteString :: BSS.ShortByteString -> SymbolName Dynamic
mkSymbolNameShortByteString str
  | not (BSS.null str) && BSS.last str == 0
  = SymbolName $ Dynamic str
  | otherwise
  = SymbolName $ Dynamic $ str `BSS.snoc` 0
