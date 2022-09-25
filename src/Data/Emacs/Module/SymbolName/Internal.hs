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
{-# LANGUAGE LambdaCase                 #-}
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
  , SymbolName(..)
  , mkSymbolName
  , mkSymbolNameString
  , mkSymbolNameShortByteString
  , mkSymbolNameUnsafe

  , mkSymbolNameCache
  , reifySymbolRaw
  , reifySymbol
  ) where

import Data.ByteString.Internal qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Coerce
import Data.IORef
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Foreign qualified as T
import Foreign.C.Types
import GHC.Exts (Addr#, unpackCString#)
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

newtype Dynamic = Dynamic { unDynamic :: Text }
  deriving (Eq, Ord, Show, Pretty)

data SymbolName
  = StaticSymbol  {-# UNPACK #-} !(Ptr CChar)
  | DynamicSymbol {-# UNPACK #-} !Text
  | CachedSymbol  (IORef (Env -> IO GlobalRef)) SymbolName
  deriving (Eq)

instance Pretty SymbolName where
  pretty = \case
    StaticSymbol (Ptr addr)
      -> pretty $ TE.decodeUtf8With TE.lenientDecode $ BS.unsafePackLiteral addr
    DynamicSymbol str  -> pretty str
    CachedSymbol _ sym -> pretty sym

-- {-# INLINE mkSymbolNameCache #-}
mkSymbolNameCache :: SymbolName -> IO (IORef (Env -> IO GlobalRef))
mkSymbolNameCache = go
  where
    go :: SymbolName -> IO (IORef (Env -> IO GlobalRef))
    go name =
      unsafeFixIO $ \ref ->
        newIORef $ \env -> do
          global <- Raw.makeGlobalRef env =<< reifySymbolRaw env name
          writeIORef ref $ \_env -> pure global
          pure global

-- | Should be applied to unboxed string literals like this
--
-- @
-- mkSymbolNameUnsafe "foo"#
-- @
--
-- Can be safely applied to non-literals (e.g. arbitrary pointers) if
-- it's guaranteed that address points to a null-terminated strings.
-- Otherwise behaviour is undefined.
{-# INLINE mkSymbolNameUnsafe #-}
mkSymbolNameUnsafe :: Addr# -> SymbolName
mkSymbolNameUnsafe addr = StaticSymbol (Ptr addr)

{-# INLINE mkSymbolName #-}
mkSymbolName :: Text -> SymbolName
mkSymbolName = DynamicSymbol

{-# INLINE mkSymbolNameShortByteString #-}
mkSymbolNameShortByteString :: BSS.ShortByteString -> SymbolName
mkSymbolNameShortByteString = DynamicSymbol . TE.decodeUtf8With TE.lenientDecode . BSS.fromShort

{-# INLINE [0] mkSymbolNameString #-}
mkSymbolNameString :: String -> SymbolName
mkSymbolNameString = mkSymbolName . T.pack

instance IsString SymbolName where
  {-# INLINE fromString #-}
  fromString = mkSymbolNameString

{-# RULES
"SymbolName string literal" forall s .
   mkSymbolNameString (unpackCString# s) = mkSymbolNameUnsafe s
 #-}

{-# INLINE reifySymbolRaw #-}
reifySymbolRaw :: Env -> SymbolName -> IO RawValue
reifySymbolRaw env sym = reifySymbol env sym id coerce

{-# INLINE reifySymbol #-}
reifySymbol :: Env -> SymbolName -> (RawValue -> a) -> (GlobalRef -> a) -> IO a
reifySymbol env sym f g = case sym of
  StaticSymbol addr ->
    f <$> Raw.intern env addr
  DynamicSymbol str ->
    f <$> T.withCString str (Raw.intern env)
  CachedSymbol ref _ ->
    g <$> (($ env) =<< readIORef ref)
