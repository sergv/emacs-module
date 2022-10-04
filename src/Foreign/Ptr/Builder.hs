----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr.Builder
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Foreign.Ptr.Builder
  ( Builder
  , withByteArrayLen
  , withPtrLen
  , withPtrUnaligned
  , storable
  , prim
  , Int#
  , ByteArray#
  ) where

import Data.Primitive.Types as Prim
import Emacs.Module.Assert
import Foreign
import Foreign.Storable as Storable
import GHC.Exts
import GHC.IO

type Writer = Addr# -> Int# -> IO ()

data Builder a = Builder Int# Writer

instance Show (Builder a) where
  showsPrec n (Builder k _) = showParen (n >= 10) (showString "Builder " . showsPrec 11 (I# k))

instance Semigroup (Builder a) where
  {-# INLINE (<>) #-}
  Builder n f <> Builder m g =
    Builder (n +# m) (\ptr off -> f ptr off *> g ptr (off +# n))

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = Builder 0# (\_ _ -> pure ())

isPowerOfTwo :: Int# -> Bool
isPowerOfTwo x = isTrue# (and# x' y' `eqWord#` 0##)
  where
    x' = int2Word# x
    y' = int2Word# (x -# 1#)

{-# INLINE withByteArrayLen #-}
withByteArrayLen
  :: forall a b. (WithCallStack, Storable a)
  => Builder a
  -> (Int# -> ByteArray# -> IO b)
  -> IO b
withByteArrayLen (Builder size f) action =
  emacsAssert (isPowerOfTwo align) "Alignment should be a power of two" $
  IO $ \s0 ->
    case newAlignedPinnedByteArray# (size *# elemSize) align s0 of
      (# s1, mbarr# #) ->
        case unIO (f (mutableByteArrayContents# mbarr#) 0#) s1 of
          (# s2, () #) ->
            case unsafeFreezeByteArray# mbarr# s2 of
              (# s3, barr# #) ->
                case action size barr# of
                  IO action' ->
                    keepAlive# barr# s3 action'
  where
    !(I# elemSize) = Storable.sizeOf    (undefined :: a)
    !(I# align)    = Storable.alignment (undefined :: a)

{-# INLINE withPtrLen #-}
withPtrLen :: forall a b. (WithCallStack, Storable a) => Builder a -> (Int -> Ptr a -> IO b) -> IO b
withPtrLen (Builder size f) action =
  emacsAssert (isPowerOfTwo align) "Alignment should be a power of two" $
  IO $ \s0 ->
    case newAlignedPinnedByteArray# (size *# elemSize) align s0 of
      (# s1, mbarr# #) ->
        case unIO (f (mutableByteArrayContents# mbarr#) 0#) s1 of
          (# s2, () #) ->
            case unsafeFreezeByteArray# mbarr# s2 of
              (# s3, barr# #) ->
                case action (I# size) (Ptr (byteArrayContents# barr#)) of
                  IO action' ->
                    keepAlive# barr# s3 action'
  where
    !(I# elemSize) = Storable.sizeOf    (undefined :: a)
    !(I# align)    = Storable.alignment (undefined :: a)

{-# INLINE withPtrUnaligned #-}
withPtrUnaligned :: forall a b. (WithCallStack, Storable a) => Builder a -> (Ptr a -> IO b) -> IO b
withPtrUnaligned (Builder size f) action =
  IO $ \s0 ->
    case newPinnedByteArray# (size *# elemSize) s0 of
      (# s1, mbarr# #) ->
        case unIO (f (mutableByteArrayContents# mbarr#) 0#) s1 of
          (# s2, () #) ->
            case unsafeFreezeByteArray# mbarr# s2 of
              (# s3, barr# #) ->
                case action (Ptr (byteArrayContents# barr#)) of
                  IO action' ->
                    keepAlive# barr# s3 action'
  where
    !(I# elemSize) = Storable.sizeOf (undefined :: a)

-- {-# INLINE storable #-}
-- storable :: Storable a => a -> Builder
-- storable x = Builder (Storable.sizeOf x) $ \ptr off -> pokeByteOff (castPtr ptr) off x

{-# INLINE storable #-}
storable :: Storable a => a -> Builder a
storable x = Builder 1# $ \addr off -> pokeElemOff (Ptr addr) (I# off) x

{-# INLINE prim #-}
prim :: Prim a => a -> Builder a
prim x = Builder 1# $ \addr off ->
  IO $ \s ->
    case Prim.writeOffAddr# addr off x s of
      s' -> (# s', () #)
