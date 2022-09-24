----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.ValueStore.Internal
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Emacs.Module.ValueStore.Internal
  ( ReleaseHandle(..)
  , ValueStore(..)
  , dummyReleaseHandle
  , new
  , add
  , forget
  , traverse_
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Data.Coerce
import Data.IORef
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U

newtype ReleaseHandle = ReleaseHandle { unReleaseHandle :: Int }
  deriving (Eq, Ord, Show, NFData, Prim)

newtype instance U.MVector s ReleaseHandle = MV_ReleaseHandle (P.MVector s ReleaseHandle)
newtype instance U.Vector    ReleaseHandle = V_ReleaseHandle  (P.Vector    ReleaseHandle)

deriving via (U.UnboxViaPrim ReleaseHandle) instance GM.MVector U.MVector ReleaseHandle
deriving via (U.UnboxViaPrim ReleaseHandle) instance G.Vector   U.Vector  ReleaseHandle

instance U.Unbox ReleaseHandle

-- | Release handle that does nothing
dummyReleaseHandle :: ReleaseHandle
dummyReleaseHandle = ReleaseHandle (-1)

data Storage a = Storage
  { sFreshIdx :: {-# UNPACK #-} !Int
  , sCapacity :: {-# UNPACK #-} !Int
  , sValues   :: {-# UNPACK #-} !(MutablePrimArray (PrimState IO) a)
  }

newtype ValueStore a = ValueStore { unValueStore :: IORef (Storage a) }

new :: forall a. Prim a => IO (ValueStore a)
new = do
  (sValues :: MutablePrimArray (PrimState IO) a) <- newPrimArray initSize
  coerce $ newIORef $ Storage
    { sFreshIdx = 0
    , sCapacity = initSize
    , sValues
    }
  where
    initSize :: Int
    initSize = 16

add :: Prim a => a -> ValueStore a -> IO ReleaseHandle
add val (ValueStore ref) = do
  s@Storage{sFreshIdx, sCapacity, sValues} <- readIORef ref
  if sFreshIdx == sCapacity
  then do
    let !sCapacity' = sCapacity * 2
    sValues' <- resizeMutablePrimArray sValues sCapacity'
    writePrimArray sValues' sFreshIdx val
    writeIORef ref $ s { sFreshIdx = sFreshIdx + 1, sCapacity = sCapacity', sValues = sValues' }
    pure $ ReleaseHandle sFreshIdx
  else do
    writePrimArray sValues sFreshIdx val
    writeIORef ref $ s { sFreshIdx = sFreshIdx + 1 }
    pure $ ReleaseHandle sFreshIdx

forget :: Prim a => ReleaseHandle -> a -> ValueStore a -> IO ()
forget (ReleaseHandle idx) val (ValueStore ref) = do
  Storage{sValues} <- readIORef ref
  writePrimArray sValues idx val

traverse_ :: Prim a => (a -> IO ()) -> ValueStore a -> IO ()
traverse_ f = traversePrimArray_ f <=< unsafeFreezePrimArray . sValues <=< readIORef . unValueStore
