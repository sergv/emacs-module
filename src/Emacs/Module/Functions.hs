----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Functions
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Wrappers around some Emacs functions, independent of concrete monad.
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Emacs.Module.Functions
  ( bindFunction
  , withCleanup
  , provide
  , makeUserPtrFromStablePtr
  , extractStablePtrFromUserPtr
    -- * Haskell<->Emacs datatype conversions
  , extractInt
  , makeInt
  , extractText
  , makeText
  , extractShortByteString
  , makeShortByteString
  , extractBool
  , makeBool
    -- * Vectors
  , extractVector
  , extractVectorWith
  , extractUnboxedVectorWith
  , makeVector
  , vconcat2
    -- * Lists
  , cons
  , car
  , cdr
  , nil
  , setcar
  , setcdr
  , makeList
  , extractList
  , extractListWith
  , extractListRevWith
  , foldlEmacsListWith
  , unfoldEmacsListWith
    -- * Strings
  , addFaceProp
  , propertize
  , concat2
  , valueToText
  , symbolName

    -- * Reexports
  , MonadMask
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Foldable
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Foreign.StablePtr
import Prettyprinter

import Data.Emacs.Module.Env qualified as Env
import Data.Emacs.Module.SymbolName
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Emacs.Module.Assert
import Emacs.Module.Monad.Class

{-# INLINABLE bindFunction #-}
-- | Assign a name to function value.
bindFunction
  :: (WithCallStack, MonadEmacs m, Monad (m s), Pretty a, UseSymbolName a)
  => SymbolName a -- ^ Name
  -> EmacsRef m s -- ^ Function value
  -> m s ()
bindFunction name def = do
  name' <- intern name
  funcallPrimitive_ (mkSymbolNameUnsafe# "fset"#) [name', def]

{-# INLINE provide #-}
-- | Signal to Emacs that certain feature is being provided. Returns provided
-- symbol.
provide
  :: (WithCallStack, MonadEmacs m, Monad (m s), Pretty a, UseSymbolName a)
  => SymbolName a -- ^ Feature to provide
  -> m s ()
provide sym = do
  sym' <- intern sym
  funcallPrimitive_ (mkSymbolNameUnsafe# "provide"#) [sym']

{-# INLINE makeUserPtrFromStablePtr #-}
-- | Pack a stable pointer as Emacs @user_ptr@.
makeUserPtrFromStablePtr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => StablePtr a
  -> m s (EmacsRef m s)
makeUserPtrFromStablePtr =
  makeUserPtr Env.freeStablePtrFinaliser . castStablePtrToPtr

{-# INLINE extractStablePtrFromUserPtr #-}
extractStablePtrFromUserPtr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> m s (StablePtr a)
extractStablePtrFromUserPtr =
  fmap castPtrToStablePtr . extractUserPtr

{-# INLINE extractInt #-}
-- | Try to obtain an 'Int' from Emacs value.
--
-- This function will fail if Emacs value is not an integer or
-- contains value too big to fit into 'Int' on current architecture.
extractInt
  :: (WithCallStack, MonadEmacs m, Monad (m s)) => EmacsRef m s -> m s Int
extractInt x = do
  y <- extractWideInteger x
  emacsAssert
    (y <= fromIntegral (maxBound :: Int))
    ("Integer is too wide to fit into Int: " ++ show y)
    (pure (fromIntegral y))

{-# INLINE makeInt #-}
-- | Pack an 'Int' integer for Emacs.
makeInt
  :: (WithCallStack, MonadEmacs m, Monad (m s)) => Int -> m s (EmacsRef m s)
makeInt = makeWideInteger . fromIntegral

{-# INLINE extractText #-}
-- | Extract string contents as 'Text' from an Emacs value.
extractText
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s -> m s Text
extractText x = TE.decodeUtf8With TE.lenientDecode <$> extractString x

{-# INLINE makeText #-}
-- | Convert a Text into an Emacs string value.
makeText
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Text -> m s (EmacsRef m s)
makeText = makeString . TE.encodeUtf8


{-# INLINE extractShortByteString #-}
-- | Extract string contents as 'ShortByteString' from an Emacs value.
extractShortByteString
  :: (WithCallStack, MonadEmacs m, Functor (m s))
  => EmacsRef m s -> m s ShortByteString
extractShortByteString = fmap BSS.toShort . extractString

{-# INLINE makeShortByteString #-}
-- | Convert a ShortByteString into an Emacs string value.
makeShortByteString
  :: (WithCallStack, MonadEmacs m)
  => ShortByteString -> m s (EmacsRef m s)
makeShortByteString = makeString . BSS.fromShort


{-# INLINE extractBool #-}
-- | Extract a boolean from an Emacs value.
extractBool
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s -> m s Bool
extractBool = isNotNil

{-# INLINE makeBool #-}
-- | Convert a Bool into an Emacs string value.
makeBool
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Bool -> m s (EmacsRef m s)
makeBool b = intern (if b then mkSymbolNameUnsafe# "t"# else mkSymbolNameUnsafe# "nil"#)

{-# INLINE withCleanup #-}
-- | Feed a value into a function and clean it up afterwards.
withCleanup
  :: (WithCallStack, MonadMask (m s), MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> (EmacsRef m s -> m s a)
  -> m s a
withCleanup x f = f x `finally` freeValue x

{-# INLINABLE extractVector #-}
-- | Get all elements form an Emacs vector.
extractVector
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s -> m s (V.Vector (EmacsRef m s))
extractVector xs = do
  n <- vecSize xs
  V.generateM n $ vecGet xs

{-# INLINABLE extractVectorWith #-}
-- | Get all elements form an Emacs vector using specific function to
-- convert elements.
extractVectorWith
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => (EmacsRef m s -> m s a)
  -> EmacsRef m s
  -> m s (V.Vector a)
extractVectorWith f xs = do
  n <- vecSize xs
  V.generateM n $ f <=< vecGet xs

{-# INLINABLE extractUnboxedVectorWith #-}
-- | Get all elements form an Emacs vector using specific function to
-- convert elements.
extractUnboxedVectorWith
  :: (WithCallStack, MonadEmacs m, Monad (m s), U.Unbox a)
  => (EmacsRef m s -> m s a)
  -> EmacsRef m s
  -> m s (U.Vector a)
extractUnboxedVectorWith f xs = do
  n <- vecSize xs
  U.generateM n $ f <=< vecGet xs

{-# INLINE makeVector #-}
-- | Create an Emacs vector.
makeVector
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => [EmacsRef m s]
  -> m s (EmacsRef m s)
makeVector = funcallPrimitive (mkSymbolNameUnsafe# "vector"#)

{-# INLINE vconcat2 #-}
-- | Concatenate two vectors.
vconcat2
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> EmacsRef m s
  -> m s (EmacsRef m s)
vconcat2 x y =
  funcallPrimitive (mkSymbolNameUnsafe# "vconcat"#) [x, y]

{-# INLINE cons #-}
-- | Make a cons pair out of two values.
cons
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s -- ^ car
  -> EmacsRef m s -- ^ cdr
  -> m s (EmacsRef m s)
cons x y = funcallPrimitive (mkSymbolNameUnsafe# "cons"#) [x, y]

{-# INLINE car #-}
-- | Take first element of a pair.
car
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> m s (EmacsRef m s)
car = funcallPrimitive (mkSymbolNameUnsafe# "car"#) . (: [])

{-# INLINE cdr #-}
-- | Take second element of a pair.
cdr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> m s (EmacsRef m s)
cdr = funcallPrimitive (mkSymbolNameUnsafe# "cdr"#) . (: [])

{-# INLINE nil #-}
-- | A @nil@ symbol aka empty list.
nil
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => m s (EmacsRef m s)
nil = intern (mkSymbolNameUnsafe# "nil"#)

{-# INLINE setcar #-}
-- | Mutate first element of a cons pair.
setcar
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s -- ^ Cons pair
  -> EmacsRef m s -- ^ New value
  -> m s ()
setcar x y = funcallPrimitive_ (mkSymbolNameUnsafe# "setcar"#) [x, y]

{-# INLINE setcdr #-}
-- | Mutate second element of a cons pair.
setcdr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s -- ^ Cons pair
  -> EmacsRef m s -- ^ New value
  -> m s ()
setcdr x y = funcallPrimitive_ (mkSymbolNameUnsafe# "setcdr"#) [x, y]

{-# INLINE makeList #-}
-- | Construct vanilla Emacs list from a Haskell list.
makeList
  :: (WithCallStack, MonadEmacs m, Monad (m s), Foldable f)
  => f (EmacsRef m s)
  -> m s (EmacsRef m s)
makeList = unfoldEmacsListWith (pure . go) . toList
  where
    go = \case
      []     -> Nothing
      y : ys -> Just (y, ys)

{-# INLINE extractList #-}
-- | Extract vanilla Emacs list as Haskell list.
extractList
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> m s [EmacsRef m s]
extractList = extractListWith pure

{-# INLINE extractListWith #-}
-- | Extract vanilla Emacs list as a Haskell list.
extractListWith
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => (EmacsRef m s -> m s a)
  -> EmacsRef m s
  -> m s [a]
extractListWith = \f -> fmap reverse . extractListRevWith f

{-# INLINE extractListRevWith #-}
-- | Extract vanilla Emacs list as a reversed Haskell list. It's more
-- efficient than 'extractList' but doesn't preserve order of elements
-- that was specified from Emacs side.
extractListRevWith
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => (EmacsRef m s -> m s a)
  -> EmacsRef m s
  -> m s [a]
extractListRevWith f = go []
  where
    go acc xs = do
      nonNil <- isNotNil xs
      if nonNil
        then do
          x   <- f =<< car xs
          xs' <- cdr xs
          go (x : acc) xs'
        else pure acc

{-# INLINE foldlEmacsListWith #-}
-- | Fold Emacs list starting from the left.
foldlEmacsListWith
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => (a -> EmacsRef m s -> m s a)
  -> a
  -> EmacsRef m s
  -> m s a
foldlEmacsListWith f = go
  where
    go acc xs = do
      nonNil <- isNotNil xs
      if nonNil
        then do
          acc' <- f acc =<< car xs
          go acc' =<< cdr xs
        else pure acc

{-# INLINE unfoldEmacsListWith #-}
-- | Fold Emacs list starting from the left.
unfoldEmacsListWith
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => (a -> m s (Maybe (EmacsRef m s, a)))
  -> a
  -> m s (EmacsRef m s)
unfoldEmacsListWith f accum = do
  accum' <- f accum
  nilVal <- nil
  case accum' of
    Nothing           -> pure nilVal
    Just (x, accum'') -> do
      cell <- cons x nilVal
      go nilVal accum'' cell
      pure cell
  where
    go nilVal = go'
      where
        go' acc cell = do
          acc' <- f acc
          case acc' of
            Nothing         -> pure ()
            Just (x, acc'') -> do
              cell' <- cons x nilVal
              setcdr cell cell'
              go' acc'' cell'

{-# INLINE addFaceProp #-}
-- | Add new 'face property to a string.
addFaceProp
  :: (WithCallStack, MonadEmacs m, Monad (m s), Pretty a, UseSymbolName a)
  => EmacsRef m s       -- ^ String to add face to
  -> SymbolName a       -- ^ Face name
  -> m s (EmacsRef m s) -- ^ Propertised string
addFaceProp str face = do
  face' <- intern face
  propertize str [(SomeSymbolName (mkSymbolNameUnsafe# "face"#), face')]

{-# INLINE propertize #-}
-- | Add new 'face property to a string.
propertize
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s                                     -- ^ String to add properties to
  -> [(SomeSymbolName EmacsSymbolName, EmacsRef m s)] -- ^ Properties
  -> m s (EmacsRef m s)                               -- ^ Propertised string
propertize str props = do
  props' <- traverse (\(SomeSymbolName name, val) -> (\name' -> [name', val]) <$> intern name) props
  funcallPrimitive (mkSymbolNameUnsafe# "propertize"#) (str : concat props')

{-# INLINE concat2 #-}
-- | Concatenate two strings.
concat2
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> EmacsRef m s
  -> m s (EmacsRef m s)
concat2 x y =
  funcallPrimitive (mkSymbolNameUnsafe# "concat"#) [x, y]

{-# INLINE valueToText #-}
-- | Convert an Emacs value into a string using @prin1-to-string@.
valueToText
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> m s Text
valueToText x =
  extractText =<< funcallPrimitive (mkSymbolNameUnsafe# "prin1-to-string"#) [x]

{-# INLINE symbolName #-}
-- | Wrapper around Emacs @symbol-name@ function - take a symbol
-- and produce an Emacs string with its textual name.
symbolName
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s
  -> m s (EmacsRef m s)
symbolName = funcallPrimitive (mkSymbolNameUnsafe# "symbol-name"#) . (:[])
