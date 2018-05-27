----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Functions
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Wrappers around some Emacs functions, independent of concrete monad.
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RankNTypes       #-}

{-# OPTIONS_HADDOCK not-home #-}

module Emacs.Module.Functions
  ( bindFunction
  , makeFunction
  , extractInt
  , makeInt
  , extractText
  , makeText
  , extractVector
  , extractVectorWith
  ) where

import Control.Monad.Except

import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Foreign.Ptr (nullPtr)

import Data.Emacs.Module.Args
import Data.Emacs.Module.SymbolName (SymbolName)
import Data.Emacs.Module.SymbolName.TH
import qualified Data.Emacs.Module.Value as Emacs
import Emacs.Module.Assert
import Emacs.Module.Monad.Class


{-# INLINABLE bindFunction #-}
-- | Assign a name to function value.
bindFunction
  :: (WithCallStack, MonadEmacs m)
  => SymbolName  -- ^ Name
  -> Emacs.Value -- ^ Function value
  -> m ()
bindFunction name def = do
  name' <- intern name
  void $ funcall [esym|fset|] [name', def]

{-# INLINE makeFunction #-}
-- | Wrap Haskell function so that it's callable from Emacs.
--
-- This is a simplified version of 'makeFunctionExtra'.
makeFunction
  :: (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest, MonadEmacs m)
  => EmacsFunction req opt rest
  -> C8.ByteString
  -> m Emacs.Value
makeFunction f doc =
  makeFunctionExtra (\env _extraPtr -> f env) doc nullPtr

{-# INLINE extractInt #-}
-- | Try to obtain an 'Int' from Emacs value.
--
-- This function will fail if Emacs value is not an integer or
-- contains value too big to fit into 'Int' on current architecture.
extractInt
  :: (WithCallStack, MonadEmacs m) => Emacs.Value -> m Int
extractInt x = do
  y <- extractWideInteger x
  emacsAssert
    (y <= fromIntegral (maxBound :: Int))
    ("Integer is too wide to fit into Int: " ++ show y)
    (pure (fromIntegral y))

{-# INLINE makeInt #-}
-- | Pack an 'Int' integer for Emacs.
makeInt
  :: (WithCallStack, MonadEmacs m) => Int -> m Emacs.Value
makeInt = makeWideInteger . fromIntegral

{-# INLINE extractText #-}
-- | Extract string contents from an Emacs value.
extractText :: (WithCallStack, MonadEmacs m) => Emacs.Value -> m Text
extractText x = TE.decodeUtf8With TE.lenientDecode <$> extractString x

{-# INLINE makeText #-}
-- | Convert a Text into an Emacs string value.
makeText :: (WithCallStack, MonadEmacs m) => Text -> m Emacs.Value
makeText = makeString . TE.encodeUtf8

{-# INLINABLE extractVector #-}
-- | Get all elements form an Emacs vector.
extractVector
  :: (WithCallStack, MonadEmacs m) => Emacs.Value -> m [Emacs.Value]
extractVector xs = do
  n <- vecSize xs
  traverse (vecGet xs) [0..n - 1]

-- | Get all elements form an Emacs vector using specific function to
-- convert elements.
extractVectorWith
  :: (WithCallStack, MonadEmacs m)
  => (Emacs.Value -> m a)
  -> Emacs.Value
  -> m [a]
extractVectorWith f xs = do
  n <- vecSize xs
  traverse (f <=< vecGet xs) [0..n - 1]
