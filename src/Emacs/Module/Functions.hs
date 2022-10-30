----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Functions
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Wrappers around some Emacs functions, independent of concrete monad.
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Emacs.Module.Functions
  ( funcallPrimitiveSym
  , funcallPrimitiveUncheckedSym
  , funcallPrimitiveSym_
  , bindFunction
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
  , extractVectorAsPrimArray
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

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Interleave
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Foldable
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Tuple.Homogenous
import Data.Vector.Unboxed qualified as U
import Foreign.StablePtr

import Data.Emacs.Module.Env qualified as Env
import Data.Emacs.Module.SymbolName
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Emacs.Module.Assert
import Emacs.Module.Monad.Class

-- | Call a function by its name, similar to 'funcallPrimitive'.
{-# INLINE funcallPrimitiveSym #-}
funcallPrimitiveSym
  :: (WithCallStack, MonadEmacs m v, Foldable f)
  => SymbolName -> f (v s) -> m s (v s)
funcallPrimitiveSym func args = do
  func' <- intern func
  funcallPrimitive func' args

-- | Call a function by its name, similar to 'funcallPrimitiveUnchecked'.
{-# INLINE funcallPrimitiveUncheckedSym #-}
funcallPrimitiveUncheckedSym
  :: (WithCallStack, MonadEmacs m v, Foldable f)
  => SymbolName -> f (v s) -> m s (v s)
funcallPrimitiveUncheckedSym func args = do
  func' <- intern func
  funcallPrimitive func' args

-- | Call a function by its name and ignore its result, similar to 'funcallPrimitiveSym'.
{-# INLINE funcallPrimitiveSym_ #-}
funcallPrimitiveSym_
  :: (WithCallStack, MonadEmacs m v, Foldable f)
  => SymbolName -> f (v s) -> m s ()
funcallPrimitiveSym_ func args =
  void $ funcallPrimitiveSym func args

{-# INLINABLE bindFunction #-}
-- | Assign a name to function value.
bindFunction
  :: (WithCallStack, MonadEmacs m v)
  => SymbolName   -- ^ Name
  -> v s -- ^ Function value
  -> m s ()
bindFunction name def = do
  name' <- intern name
  funcallPrimitiveSym_ Sym.fset [name', def]

{-# INLINE provide #-}
-- | Signal to Emacs that certain feature is being provided. Returns provided
-- symbol.
provide
  :: (WithCallStack, MonadEmacs m v)
  => SymbolName -- ^ Feature to provide
  -> m s ()
provide sym = do
  sym' <- intern sym
  void $ funcallPrimitiveUncheckedSym Sym.provide [sym']

{-# INLINE makeUserPtrFromStablePtr #-}
-- | Pack a stable pointer as Emacs @user_ptr@.
makeUserPtrFromStablePtr
  :: (WithCallStack, MonadEmacs m v)
  => StablePtr a
  -> m s (v s)
makeUserPtrFromStablePtr =
  makeUserPtr Env.freeStablePtrFinaliser . castStablePtrToPtr

{-# INLINE extractStablePtrFromUserPtr #-}
extractStablePtrFromUserPtr
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> m s (StablePtr a)
extractStablePtrFromUserPtr =
  fmap castPtrToStablePtr . extractUserPtr

{-# INLINE extractInt #-}
-- | Try to obtain an 'Int' from Emacs value.
--
-- This function will fail if Emacs value is not an integer or
-- contains value too big to fit into 'Int' on current architecture.
extractInt
  :: (WithCallStack, MonadEmacs m v) => v s -> m s Int
extractInt x = do
  y <- extractWideInteger x
  emacsAssert
    (y <= fromIntegral (maxBound :: Int))
    ("Integer is too wide to fit into Int: " ++ show y)
    (pure (fromIntegral y))

{-# INLINE makeInt #-}
-- | Pack an 'Int' integer for Emacs.
makeInt
  :: (WithCallStack, MonadEmacs m v) => Int -> m s (v s)
makeInt = makeWideInteger . fromIntegral

{-# INLINE extractText #-}
-- | Extract string contents as 'Text' from an Emacs value.
extractText
  :: (WithCallStack, MonadEmacs m v)
  => v s -> m s Text
extractText x = TE.decodeUtf8With TE.lenientDecode <$> extractString x

{-# INLINE makeText #-}
-- | Convert a Text into an Emacs string value.
makeText
  :: (WithCallStack, MonadEmacs m v)
  => Text -> m s (v s)
makeText = makeString . TE.encodeUtf8


{-# INLINE extractShortByteString #-}
-- | Extract string contents as 'ShortByteString' from an Emacs value.
extractShortByteString
  :: (WithCallStack, MonadEmacs m v)
  => v s -> m s ShortByteString
extractShortByteString = fmap BSS.toShort . extractString

{-# INLINE makeShortByteString #-}
-- | Convert a ShortByteString into an Emacs string value.
makeShortByteString
  :: (WithCallStack, MonadEmacs m v)
  => ShortByteString -> m s (v s)
makeShortByteString = makeString . BSS.fromShort


{-# INLINE extractBool #-}
-- | Extract a boolean from an Emacs value.
extractBool
  :: (WithCallStack, MonadEmacs m v)
  => v s -> m s Bool
extractBool = isNotNil

{-# INLINE makeBool #-}
-- | Convert a Bool into an Emacs string value.
makeBool
  :: (WithCallStack, MonadEmacs m v)
  => Bool -> m s (v s)
makeBool b = if b then intern Sym.t else nil

{-# INLINE extractVector #-}
-- | Get all elements form an Emacs vector.
extractVector
  :: (WithCallStack, MonadEmacs m v, U.Unbox (v s))
  => v s -> m s (U.Vector (v s))
extractVector xs = do
  n <- vecSize xs
  U.generateM n $ vecGet xs

{-# INLINE extractVectorAsPrimArray #-}
-- | Get all elements form an Emacs vector.
extractVectorAsPrimArray
  :: (WithCallStack, MonadEmacs m v, Prim (v s))
  => v s -> m s (PrimArray (v s))
extractVectorAsPrimArray xs = do
  n <- vecSize xs
  generatePrimArrayA n $ vecGet xs

{-# INLINE makeVector #-}
-- | Create an Emacs vector.
makeVector
  :: (WithCallStack, MonadEmacs m v, Foldable f)
  => f (v s)
  -> m s (v s)
makeVector = funcallPrimitiveUncheckedSym Sym.vector

{-# INLINE vconcat2 #-}
-- | Concatenate two vectors.
vconcat2
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> v s
  -> m s (v s)
vconcat2 x y =
  funcallPrimitiveSym Sym.vconcat (Tuple2 (x, y))

{-# INLINE cons #-}
-- | Make a cons pair out of two values.
cons
  :: (WithCallStack, MonadEmacs m v)
  => v s -- ^ car
  -> v s -- ^ cdr
  -> m s (v s)
cons x y = funcallPrimitiveUncheckedSym Sym.cons (Tuple2 (x, y))

{-# INLINE car #-}
-- | Take first element of a pair.
car
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> m s (v s)
car = funcallPrimitiveSym Sym.car . Tuple1

{-# INLINE cdr #-}
-- | Take second element of a pair.
cdr
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> m s (v s)
cdr = funcallPrimitiveSym Sym.cdr . Tuple1

{-# INLINE nil #-}
-- | A @nil@ symbol aka empty list.
nil
  :: (WithCallStack, MonadEmacs m v)
  => m s (v s)
nil = intern Sym.nil

{-# INLINE setcar #-}
-- | Mutate first element of a cons pair.
setcar
  :: (WithCallStack, MonadEmacs m v)
  => v s -- ^ Cons pair
  -> v s -- ^ New value
  -> m s ()
setcar x y = funcallPrimitiveSym_ Sym.setcar (Tuple2 (x, y))

{-# INLINE setcdr #-}
-- | Mutate second element of a cons pair.
setcdr
  :: (WithCallStack, MonadEmacs m v)
  => v s -- ^ Cons pair
  -> v s -- ^ New value
  -> m s ()
setcdr x y = funcallPrimitiveSym_ Sym.setcdr (Tuple2 (x, y))

{-# INLINE makeList #-}
-- | Construct vanilla Emacs list from a Haskell list.
makeList
  :: (WithCallStack, MonadEmacs m v, Foldable f)
  => f (v s)
  -> m s (v s)
makeList = unfoldEmacsListWith (pure . go) . toList
  where
    go = \case
      []     -> Nothing
      y : ys -> Just (y, ys)

{-# INLINE extractList #-}
-- | Extract vanilla Emacs list as Haskell list.
extractList
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> m s [v s]
extractList = extractListWith pure

{-# INLINE extractListWith #-}
-- | Extract vanilla Emacs list as a Haskell list.
extractListWith
  :: (WithCallStack, MonadEmacs m v)
  => (v s -> m s a)
  -> v s
  -> m s [a]
extractListWith f = go
  where
    go xs = unsafeInterleave $ do
      nonNil <- isNotNil xs
      if nonNil
      then
        (:) <$> (f =<< car xs) <*> (go =<< cdr xs)
      else
        pure []

{-# INLINE foldlEmacsListWith #-}
-- | Fold Emacs list starting from the left.
foldlEmacsListWith
  :: (WithCallStack, MonadEmacs m v)
  => (a -> v s -> m s a)
  -> a
  -> v s
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
  :: (WithCallStack, MonadEmacs m v)
  => (a -> m s (Maybe (v s, a)))
  -> a
  -> m s (v s)
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
          f acc >>= \case
            Nothing         -> pure ()
            Just (x, acc'') -> do
              cell' <- cons x nilVal
              setcdr cell cell'
              go' acc'' cell'

{-# INLINE addFaceProp #-}
-- | Add new 'face property to a string.
addFaceProp
  :: (WithCallStack, MonadEmacs m v)
  => v s       -- ^ String to add face to
  -> SymbolName         -- ^ Face name
  -> m s (v s) -- ^ Propertised string
addFaceProp str face = do
  face' <- intern face
  propertize str [(Sym.face, face')]

{-# INLINE propertize #-}
-- | Add new 'face property to a string.
propertize
  :: (WithCallStack, MonadEmacs m v)
  => v s                 -- ^ String to add properties to
  -> [(SymbolName, v s)] -- ^ Properties
  -> m s (v s)           -- ^ Propertised string
propertize str props = do
  props' <- traverse (\(name, val) -> (\name' -> [name', val]) <$> intern name) props
  funcallPrimitiveSym Sym.propertize (str : concat props')

{-# INLINE concat2 #-}
-- | Concatenate two strings.
concat2
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> v s
  -> m s (v s)
concat2 x y =
  funcallPrimitiveSym Sym.concat (Tuple2 (x, y))

{-# INLINE valueToText #-}
-- | Convert an Emacs value into a string using @prin1-to-string@.
valueToText
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> m s Text
valueToText =
  extractText <=< funcallPrimitiveUncheckedSym Sym.prin1ToString . Tuple1

{-# INLINE symbolName #-}
-- | Wrapper around Emacs @symbol-name@ function - take a symbol
-- and produce an Emacs string with its textual name.
symbolName
  :: (WithCallStack, MonadEmacs m v)
  => v s
  -> m s (v s)
symbolName = funcallPrimitiveSym Sym.symbolName . Tuple1
