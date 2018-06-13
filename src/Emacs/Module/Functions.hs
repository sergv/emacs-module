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
  , provide
  , extractInt
  , makeInt
  , extractText
  , makeText
    -- * Vectors
  , extractVector
  , extractVectorWith
  , makeVector
    -- * Lists
  , cons
  , car
  , cdr
  , nil
  , setcar
  , setcdr
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
  void $ funcallPrimitive [esym|fset|] [name', def]

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

{-# INLINE provide #-}
-- | Signal to Emacs that certain feature is being provided. Returns provided
-- symbol.
provide
  :: (WithCallStack, MonadEmacs m)
  => SymbolName -- ^ Feature to provide
  -> m Emacs.Value
provide sym = do
  sym' <- intern sym
  funcallPrimitive [esym|provide|] [sym']

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

{-# INLINABLE extractVectorWith #-}
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

{-# INLINE makeVector #-}
-- | Create an Emacs vector.
makeVector
  :: (WithCallStack, MonadEmacs m)
  => [Emacs.Value]
  -> m Emacs.Value
makeVector = funcallPrimitive [esym|vector|]

{-# INLINE cons #-}
-- | Make a cons pair out of two values.
cons
  :: (WithCallStack, MonadEmacs m)
  => Emacs.Value -- ^ car
  -> Emacs.Value -- ^ cdr
  -> m Emacs.Value
cons x y = funcallPrimitive [esym|cons|] [x, y]

{-# INLINE car #-}
-- | Take first element of a pair.
car
  :: (WithCallStack, MonadEmacs m)
  => Emacs.Value
  -> m Emacs.Value
car = funcallPrimitive [esym|car|] . (: [])

{-# INLINE cdr #-}
-- | Take second element of a pair.
cdr
  :: (WithCallStack, MonadEmacs m)
  => Emacs.Value
  -> m Emacs.Value
cdr = funcallPrimitive [esym|cdr|] . (: [])

{-# INLINE nil #-}
-- | A @nil@ symbol aka empty list.
nil
  :: (WithCallStack, MonadEmacs m)
  => m Emacs.Value
nil = intern [esym|nil|]

{-# INLINE setcar #-}
-- | Mutate first element of a cons pair.
setcar
  :: (WithCallStack, MonadEmacs m)
  => Emacs.Value -- ^ Cons pair
  -> Emacs.Value -- ^ New value
  -> m Emacs.Value
setcar x y = funcallPrimitive [esym|setcar|] [x, y]

{-# INLINE setcdr #-}
-- | Mutate second element of a cons pair.
setcdr
  :: (WithCallStack, MonadEmacs m)
  => Emacs.Value -- ^ Cons pair
  -> Emacs.Value -- ^ New value
  -> m Emacs.Value
setcdr x y = funcallPrimitive [esym|setcdr|] [x, y]
