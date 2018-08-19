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
  , extractListRev
    -- * Strings
  , addFaceProp
  , concat2
  , valueToText

    -- * Reexports
  , MonadMask
  ) where

import Control.Monad.Catch
import Control.Monad.Except

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Foreign.Ptr (nullPtr)
import Foreign.StablePtr

import Data.Emacs.Module.Args
import qualified Data.Emacs.Module.Env as Env
import Data.Emacs.Module.SymbolName (SymbolName)
import Data.Emacs.Module.SymbolName.TH
import Data.Emacs.Module.Value
import Emacs.Module.Assert
import Emacs.Module.Monad.Class


{-# INLINABLE bindFunction #-}
-- | Assign a name to function value.
bindFunction
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => SymbolName -- ^ Name
  -> Value s    -- ^ Function value
  -> m s ()
bindFunction name def = do
  name' <- intern name
  funcallPrimitive_ [esym|fset|] [name', def]

{-# INLINE makeFunction #-}
-- | Make Haskell function available as an anonymoucs Emacs
-- function. In order to be able to use it later from Emacs it should
-- be fed into 'bindFunction'.
--
-- This is a simplified version of 'makeFunctionExtra'.
makeFunction
  :: (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest, MonadEmacs m, Monad (m s))
  => (forall s'. EmacsFunction req opt rest s' m)
  -> C8.ByteString
  -> m s (Value s)
makeFunction f doc =
  makeFunctionExtra (\env _extraPtr -> f env) doc nullPtr

{-# INLINE provide #-}
-- | Signal to Emacs that certain feature is being provided. Returns provided
-- symbol.
provide
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => SymbolName -- ^ Feature to provide
  -> m s (Value s)
provide sym = do
  sym' <- intern sym
  funcallPrimitive [esym|provide|] [sym']

{-# INLINE makeUserPtrFromStablePtr #-}
-- | Pack a stable pointer as Emacs @user_ptr@.
makeUserPtrFromStablePtr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => StablePtr a
  -> m s (Value s)
makeUserPtrFromStablePtr =
  makeUserPtr Env.freeStablePtrFinaliser . castStablePtrToPtr

{-# INLINE extractStablePtrFromUserPtr #-}
extractStablePtrFromUserPtr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> m s (StablePtr a)
extractStablePtrFromUserPtr =
  fmap castPtrToStablePtr . extractUserPtr

{-# INLINE extractInt #-}
-- | Try to obtain an 'Int' from Emacs value.
--
-- This function will fail if Emacs value is not an integer or
-- contains value too big to fit into 'Int' on current architecture.
extractInt
  :: (WithCallStack, MonadEmacs m, Monad (m s)) => Value s -> m s Int
extractInt x = do
  y <- extractWideInteger x
  emacsAssert
    (y <= fromIntegral (maxBound :: Int))
    ("Integer is too wide to fit into Int: " ++ show y)
    (pure (fromIntegral y))

{-# INLINE makeInt #-}
-- | Pack an 'Int' integer for Emacs.
makeInt
  :: (WithCallStack, MonadEmacs m, Monad (m s)) => Int -> m s (Value s)
makeInt = makeWideInteger . fromIntegral

{-# INLINE extractText #-}
-- | Extract string contents as 'Text' from an Emacs value.
extractText :: (WithCallStack, MonadEmacs m, Monad (m s)) => Value s -> m s Text
extractText x = TE.decodeUtf8With TE.lenientDecode <$> extractString x

{-# INLINE makeText #-}
-- | Convert a Text into an Emacs string value.
makeText :: (WithCallStack, MonadEmacs m, Monad (m s)) => Text -> m s (Value s)
makeText = makeString . TE.encodeUtf8


{-# INLINE extractShortByteString #-}
-- | Extract string contents as 'ShortByteString' from an Emacs value.
extractShortByteString
  :: (WithCallStack, MonadEmacs m, Functor (m s))
  => Value s -> m s ShortByteString
extractShortByteString = fmap BSS.toShort . extractString

{-# INLINE makeShortByteString #-}
-- | Convert a ShortByteString into an Emacs string value.
makeShortByteString
  :: (WithCallStack, MonadEmacs m)
  => ShortByteString -> m s (Value s)
makeShortByteString = makeString . BSS.fromShort


{-# INLINE extractBool #-}
-- | Extract a boolean from an Emacs value.
extractBool :: (WithCallStack, MonadEmacs m, Monad (m s)) => Value s -> m s Bool
extractBool = isNotNil

{-# INLINE makeBool #-}
-- | Convert a Bool into an Emacs string value.
makeBool :: (WithCallStack, MonadEmacs m, Monad (m s)) => Bool -> m s (Value s)
makeBool b = intern (if b then [esym|t|] else [esym|nil|])

{-# INLINE withCleanup #-}
-- | Feed a value into a function and clean it up afterwards.
withCleanup
  :: (WithCallStack, MonadMask (m s), MonadEmacs m, Monad (m s))
  => Value s
  -> (Value s -> m s a)
  -> m s a
withCleanup x f = f x `finally` freeValue x

{-# INLINABLE extractVector #-}
-- | Get all elements form an Emacs vector.
extractVector
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s -> m s (V.Vector (Value s))
extractVector xs = do
  n <- vecSize xs
  V.generateM n $ vecGet xs

{-# INLINABLE extractVectorWith #-}
-- | Get all elements form an Emacs vector using specific function to
-- convert elements.
extractVectorWith
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => (Value s -> m s a)
  -> Value s
  -> m s (V.Vector a)
extractVectorWith f xs = do
  n <- vecSize xs
  V.generateM n $ f <=< vecGet xs

{-# INLINABLE extractUnboxedVectorWith #-}
-- | Get all elements form an Emacs vector using specific function to
-- convert elements.
extractUnboxedVectorWith
  :: (WithCallStack, MonadEmacs m, Monad (m s), U.Unbox a)
  => (Value s -> m s a)
  -> Value s
  -> m s (U.Vector a)
extractUnboxedVectorWith f xs = do
  n <- vecSize xs
  U.generateM n $ f <=< vecGet xs

{-# INLINE makeVector #-}
-- | Create an Emacs vector.
makeVector
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => [Value s]
  -> m s (Value s)
makeVector = funcallPrimitive [esym|vector|]

{-# INLINE vconcat2 #-}
-- | Concatenate two vectors.
vconcat2
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> Value s
  -> m s (Value s)
vconcat2 x y =
  funcallPrimitive [esym|vconcat|] [x, y]

{-# INLINE cons #-}
-- | Make a cons pair out of two values.
cons
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s -- ^ car
  -> Value s -- ^ cdr
  -> m s (Value s)
cons x y = funcallPrimitive [esym|cons|] [x, y]

{-# INLINE car #-}
-- | Take first element of a pair.
car
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> m s (Value s)
car = funcallPrimitive [esym|car|] . (: [])

{-# INLINE cdr #-}
-- | Take second element of a pair.
cdr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> m s (Value s)
cdr = funcallPrimitive [esym|cdr|] . (: [])

{-# INLINE nil #-}
-- | A @nil@ symbol aka empty list.
nil
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => m s (Value s)
nil = intern [esym|nil|]

{-# INLINE setcar #-}
-- | Mutate first element of a cons pair.
setcar
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s -- ^ Cons pair
  -> Value s -- ^ New value
  -> m s (Value s)
setcar x y = funcallPrimitive [esym|setcar|] [x, y]

{-# INLINE setcdr #-}
-- | Mutate second element of a cons pair.
setcdr
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s -- ^ Cons pair
  -> Value s -- ^ New value
  -> m s (Value s)
setcdr x y = funcallPrimitive [esym|setcdr|] [x, y]

{-# INLINE makeList #-}
-- | Construct vanilla Emacs list from a Haskell list.
makeList
  :: (WithCallStack, MonadEmacs m, Monad (m s), Foldable f)
  => f (Value s)
  -> m s (Value s)
makeList xs = do
  nil' <- nil
  go nil' $ reverse $ toList xs
  where
    go end []       = pure end
    go end (y : ys) = do
      end' <- cons y end
      go end' ys

{-# INLINE extractList #-}
-- | Extract vanilla Emacs list as Haskell list.
extractList
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> m s [Value s]
extractList = fmap reverse . extractListRev

{-# INLINE extractListRev #-}
-- | Extract vanilla Emacs list as a reversed Haskell list. It's more
-- efficient than 'extractList' but doesn't preserve order of elements
-- that was specified from Emacs side.
extractListRev
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> m s [Value s]
extractListRev = go []
  where
    go acc xs = do
      nonNil <- isNotNil xs
      if nonNil
        then do
          x   <- car xs
          xs' <- cdr xs
          go (x : acc) xs'
        else pure acc

{-# INLINE addFaceProp #-}
-- | Add new 'face property to a string.
addFaceProp
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s       -- ^ String to add face to
  -> SymbolName    -- ^ Face name
  -> m s (Value s) -- ^ Propertised string
addFaceProp str face = do
  faceSym  <- intern [esym|face|]
  face'    <- intern face
  funcallPrimitive [esym|propertize|] [str, faceSym, face']

{-# INLINE concat2 #-}
-- | Concatenate two strings.
concat2
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> Value s
  -> m s (Value s)
concat2 x y =
  funcallPrimitive [esym|concat|] [x, y]

-- | Convert an Emacs value into a string using @prin1-to-string@.
valueToText
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => Value s
  -> m s Text
valueToText x =
  extractText =<< funcallPrimitive [esym|prin1-to-string|] [x]
