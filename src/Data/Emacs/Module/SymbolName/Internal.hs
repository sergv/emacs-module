----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecursiveDo                #-}
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
  , cacheSymbolName
  , mkSymbolName
  , mkSymbolNameUnsafe#
  , mkSymbolNameShortByteString
  ) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.IORef
import Data.Kind
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Foreign.C.Types
import GHC.Exts (Addr#, byteArrayContents#)
import GHC.Ptr
import Prettyprinter
import System.IO.Unsafe

import Data.Emacs.Module.Raw.Env qualified as Raw
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value

-- | Symbols that are known at compile time.
--
-- Will just pass pointer to 0-terminated statically-allocated string
-- to Emacs API when used.
newtype Static = Static { unStatic :: Ptr CChar }
  deriving (Eq, Ord, Show)

newtype Dynamic = Dynamic { unDynamic :: BSS.ShortByteString }
  deriving (Eq, Ord, Show)

newtype SymbolName a = SymbolName a
  deriving (Eq, Ord, Show, Pretty, Functor, Foldable, Traversable)

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

newtype Cached = Cached { _unCached :: IORef (Env -> IO GlobalRef) }
  deriving (Eq)

cacheSymbolName :: forall a. UseSymbolName a => SymbolName a -> SymbolName Cached
cacheSymbolName = unsafePerformIO . go
  where
    go :: SymbolName a -> IO (SymbolName Cached)
    go name = do
      res <- unsafeFixIO $ \ref ->
        newIORef $ \env -> do
          glob <- Raw.makeGlobalRef env =<< reifySymbol env name
          writeIORef ref $ \_env -> pure glob
          pure glob
      pure $ SymbolName $ Cached res

class UseSymbolName a where
  reifySymbol :: Env -> SymbolName a -> IO RawValue

instance UseSymbolName Static where
  {-# INLINE reifySymbol #-}
  reifySymbol env (SymbolName (Static addr)) =
    Raw.intern env addr

instance UseSymbolName Dynamic where
  -- I like to live dangerously...
  {-# INLINE reifySymbol #-}
  reifySymbol env (SymbolName (Dynamic (BSS.SBS arr))) =
    Raw.intern env (Ptr (byteArrayContents# arr))

instance UseSymbolName Cached where
  {-# INLINE reifySymbol #-}
  reifySymbol env (SymbolName (Cached ref)) = do
    unGlobalRef <$> (($ env) =<< readIORef ref)

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
