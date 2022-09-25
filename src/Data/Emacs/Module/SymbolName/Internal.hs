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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnliftedNewtypes           #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.SymbolName.Internal
  ( Static(..)
  , Dynamic(..)
  , Cached(..)
  , SymbolName(..)
  , SomeSymbolName(..)
  , cacheSymbolName
  , mkSymbolName
  , mkSymbolNameUnsafe#
  , mkSymbolNameShortByteString

  , UseSymbolName(..)
  ) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.IORef
import Data.Kind
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Foreign.C.Types
import GHC.Exts (Addr#, byteArrayContents#, proxy#)
import GHC.Ptr
import GHC.TypeLits
import Prettyprinter
import System.IO.Unsafe

import Data.Emacs.Module.GetRawValue
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

newtype Cached (name :: Symbol) = Cached { unCached :: IORef (Env -> IO GlobalRef) }
  deriving (Eq)

instance KnownSymbol name => Pretty (Cached name) where
  pretty _ = pretty $ symbolVal' (proxy# @name)

cacheSymbolName :: forall sym. SymbolName Static -> SymbolName (Cached sym)
cacheSymbolName = unsafePerformIO . go
  where
    go :: SymbolName Static -> IO (SymbolName (Cached sym))
    go name = do
      res <- unsafeFixIO $ \ref ->
        newIORef $ \env -> do
          global <- Raw.makeGlobalRef env =<< reifySymbol env name
          writeIORef ref $ \_env -> pure global
          pure global
      pure $ SymbolName $ Cached res

{-# INLINE mkSymbolName #-}
mkSymbolName :: C8.ByteString -> SymbolName Dynamic
mkSymbolName = mkSymbolNameShortByteString . BSS.toShort

data SomeSymbolName (c :: Type -> Constraint) = forall a. c a => SomeSymbolName (SymbolName a)

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

class GetRawValue (ReifiedSymbol a) => UseSymbolName a where
  type ReifiedSymbol a :: Type
  reifySymbol :: Env -> SymbolName a -> IO (ReifiedSymbol a)

instance UseSymbolName Static where
  type ReifiedSymbol Static = RawValue
  {-# INLINE reifySymbol #-}
  reifySymbol env (SymbolName (Static addr)) =
    Raw.intern env addr

instance UseSymbolName Dynamic where
  type ReifiedSymbol Dynamic = RawValue
  {-# INLINE reifySymbol #-}
  reifySymbol env (SymbolName (Dynamic (BSS.SBS arr))) =
    -- I like to live dangerously...
    Raw.intern env (Ptr (byteArrayContents# arr))

instance KnownSymbol name => UseSymbolName (Cached name) where
  type ReifiedSymbol (Cached _) = GlobalRef
  {-# INLINE reifySymbol #-}
  reifySymbol env (SymbolName (Cached ref)) =
    ($ env) =<< readIORef ref
