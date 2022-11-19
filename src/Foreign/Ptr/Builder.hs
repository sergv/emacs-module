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
  -- , withPtrUnaligned
  , storable
  , prim
  , Int#
  , ByteArray#

  , BuilderCache
  , coerceBuilderCache
  , withBuilderCache
  ) where

import Data.Array.Byte
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
  => BuilderCache a
  -> Builder a
  -> (Int# -> ByteArray# -> IO b)
  -> IO b
withByteArrayLen (BuilderCache (MutableByteArray cache#)) (Builder size f) action =
  emacsAssert (isPowerOfTwo align) "Alignment should be a power of two" $
  IO $ \s0 ->
    case getSizeofMutableByteArray# cache# s0 of
      (# s1, cacheSize #) ->
        if isTrue# (cacheSize >=# requiredSize)
        then
          case unIO (f (mutableByteArrayContents# cache#) 0#) s1 of
            (# s2, () #) ->
              case unsafeFreezeByteArray# cache# s2 of
                (# s3, barr# #) ->
                  unIO (action size barr#) s3
        else
          case newAlignedPinnedByteArray# requiredSize align s1 of
            (# s2, mbarr# #) ->
              case unIO (f (mutableByteArrayContents# mbarr#) 0#) s2 of
                (# s3, () #) ->
                  case unsafeFreezeByteArray# mbarr# s3 of
                    (# s4, barr# #) ->
                      case action size barr# of
                        IO action' ->
                          keepAlive# barr# s4 action'
  where
    !requiredSize  = size *# elemSize
    !(I# elemSize) = Storable.sizeOf    (undefined :: a)
    !(I# align)    = Storable.alignment (undefined :: a)

{-# INLINE withPtrLen #-}
withPtrLen
  :: forall a b. (WithCallStack, Storable a)
  => BuilderCache a -> Builder a -> (Int -> Ptr a -> IO b) -> IO b
withPtrLen cache b action =
  withByteArrayLen cache b $ \size barr ->
    action (I# size) (Ptr (byteArrayContents# barr))


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


newtype BuilderCache a = BuilderCache { _unBuilderCache :: MutableByteArray RealWorld }

coerceBuilderCache :: BuilderCache a -> BuilderCache b
coerceBuilderCache = coerce

withBuilderCache :: forall a b. Storable a => Int -> (BuilderCache a -> IO b) -> IO b
withBuilderCache (I# size) f = do
  IO $ \s0 ->
    case newAlignedPinnedByteArray# (size *# elemSize) align s0 of
      (# s1, mbarr #) ->
        unIO (f (BuilderCache (MutableByteArray mbarr))) s1
  where
    !(I# elemSize) = Storable.sizeOf    (undefined :: a)
    !(I# align)    = Storable.alignment (undefined :: a)
