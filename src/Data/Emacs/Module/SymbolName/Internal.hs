----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.SymbolName.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnliftedNewtypes #-}

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
  , mkCachedSymbolName
  , reifySymbolRaw
  , reifySymbolUnknown
  , reifySymbol
  ) where

import Data.ByteString.Internal qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Char
import Data.Coerce
import Data.IORef
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Foreign qualified as T
import Foreign.C.Types
import Foreign.Storable
import GHC.Exts (Addr#, unpackCString#)
import GHC.Ptr
import Prettyprinter
import System.IO.Unsafe

import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env qualified as Raw
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value
import Emacs.Module.Assert

import Data.Emacs.Module.SymbolName.Predefined.Funcall

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
  | CachedSymbol  (IORef (Env -> IO (RawValue 'Pinned))) SymbolName
  deriving (Eq)

instance Show SymbolName where
  show = \case
    StaticSymbol (Ptr addr)
      -> show $ TE.decodeUtf8With TE.lenientDecode $ BS.unsafePackLiteral addr
    DynamicSymbol str  -> show str
    CachedSymbol _ sym -> show sym

instance Pretty SymbolName where
  pretty = \case
    StaticSymbol (Ptr addr)
      -> pretty $ TE.decodeUtf8With TE.lenientDecode $ BS.unsafePackLiteral addr
    DynamicSymbol str  -> pretty str
    CachedSymbol _ sym -> pretty sym

mkSymbolNameCache :: SymbolName -> IO (IORef (Env -> IO (RawValue 'Pinned)))
mkSymbolNameCache = go
  where
    go :: SymbolName -> IO (IORef (Env -> IO (RawValue 'Pinned)))
    go !name =
      unsafeFixIO $ \ ref ->
        newIORef $ \env -> do
          !global <- Raw.makeGlobalRef env =<< reifySymbolRaw env name
          writeIORef ref $ \_env -> pure global
          pure global

{-# INLINE mkCachedSymbolName #-}
mkCachedSymbolName :: IORef (Env -> IO (RawValue 'Pinned)) -> SymbolName -> SymbolName
mkCachedSymbolName = CachedSymbol

-- | Should be applied to unboxed string literals like this
--
-- @
-- mkSymbolNameUnsafe "foo"#
-- @
--
-- Can be safely applied to non-literals (e.g. arbitrary pointers) if
-- it's guaranteed that address points to a null-terminated strings.
-- Otherwise behaviour is undefined.
--
-- The string literal must only contain ASCII symbols. This condition
-- is required by the Emacs API and results in undefined behaviour if
-- violated.
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
reifySymbolRaw :: Env -> SymbolName -> IO (RawValue 'Regular)
reifySymbolRaw env sym = reifySymbol env sym id coerce

{-# INLINE reifySymbolUnknown #-}
reifySymbolUnknown :: Env -> SymbolName -> IO (RawValue 'Unknown)
reifySymbolUnknown env sym = reifySymbol env sym coerce coerce

{-# INLINE reifySymbol #-}
reifySymbol
  :: WithCallStack
  => Env -> SymbolName -> (RawValue 'Regular -> a) -> (RawValue 'Pinned -> a) -> IO a
reifySymbol env sym f g = case sym of
  StaticSymbol addr ->
    f <$> Raw.intern env addr
  DynamicSymbol str
    -- If it's only ASCII then can run FFI intern, otherwise have to go via funcall
    -- TODO: cache this check
    | T.all (\c -> ord c < 128) str ->
      f <$> T.withCString str (Raw.intern env)
    | otherwise                    ->
      T.withCStringLen str $ \(ptr, len) -> do
        str' <- emacsAssert (len >= 0) "Symbol text length must be non-negative" $
          Raw.makeString env ptr (fromIntegral len)
        funcall' <- reifySymbolUnknown env funcall
        allocaNonNull $ \args -> do
          poke (unNonNullPtr args) str'
          f <$> Raw.funcallPrimitive env funcall' 1 args

  CachedSymbol ref _ ->
    g <$> ((\k -> k env) =<< readIORef ref)
