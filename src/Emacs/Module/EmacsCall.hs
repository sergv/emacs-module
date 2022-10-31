----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.EmacsCall
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Emacs.Module.EmacsCall
  ( EmacsCall(..)
  ) where

import Data.ByteString qualified as BS
import Data.Int
import Data.Kind
import Data.Void
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Stack (CallStack)

import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal
import Emacs.Module.Errors
import Foreign.Ptr.Builder as PtrBuilder

-- NB: all RawValues are stored without bangs so that they may lazily
-- be loaded by emacs. Otherwise client thread will block until the
-- actual Emacs value pointer becomes available and we don’t want that
-- since client thread may have other productive work to do in the
-- meantime.

-- | @f@ is a "failure" wrapper for results that may include error info.
data EmacsCall (f :: Type -> Type -> Type -> Type) (out :: Type -> Type) (res :: Type) where
  MakeGlobalRef
    :: RawValue 'Regular
    -> out (RawValue 'Pinned)
    -> EmacsCall f out (RawValue 'Pinned)
  FreeGlobalRef
    :: RawValue 'Pinned
    -> EmacsCall f out ()

  NonLocalExitCheck
    :: out (FuncallExit ())
    -> EmacsCall f out (FuncallExit ())
  NonLocalExitGet
    :: out (FuncallExit (RawValue 'Regular, RawValue 'Regular))
    -> EmacsCall f out (FuncallExit (RawValue 'Regular, RawValue 'Regular))
  NonLocalExitSignal
    :: CallStack
    -> RawValue 'Unknown           -- ^ Error symbol
    -> Builder (RawValue 'Regular) -- ^ Error data
    -> out EmacsSignal
    -> EmacsCall f out EmacsSignal
  NonLocalExitThrow
    :: RawValue p1 -- ^ Tag, a symbol
    -> RawValue p2 -- ^ Value
    -> EmacsCall f out a
  NonLocalExitClear
    :: EmacsCall f out ()

  MakeFunction
    :: !Int                      -- ^ Minimum arity
    -> !Int                      -- ^ Maximum arity
    -> !(RawFunction 'Unknown a) -- ^ Implementation
    -> !Doc.Doc                  -- ^ Documentation
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)
  Funcall
    -- -> Int#                -- ^ Number of arguments
    -- -> ByteArray#          -- ^ Arguments, must be pinned
    :: RawValue 'Regular           -- ^ Function
    -> Builder (RawValue 'Regular) -- ^ Arguments, lazy to not make client
                                   -- sending request block until argument
                                   -- values become available
    -> out (f EmacsSignal EmacsThrow (RawValue 'Regular))
    -> EmacsCall f out (f EmacsSignal EmacsThrow (RawValue 'Regular))
  FuncallPrimitive
    -- -> Int#                -- ^ Number of arguments
    -- -> ByteArray#          -- ^ Arguments, must be pinned
    :: RawValue 'Regular           -- ^ Function
    -> Builder (RawValue 'Regular) -- ^ Arguments, lazy to not make client
                                   -- sending request block until argument
                                   -- values become available
    -> out (f EmacsSignal EmacsThrow (RawValue 'Regular))
    -> EmacsCall f out (f EmacsSignal EmacsThrow (RawValue 'Regular))
  -- | For functions that cannot fail
  FuncallPrimitiveUnchecked
    :: RawValue 'Regular           -- ^ Function
    -> Builder (RawValue 'Regular) -- ^ Arguments, lazy to not make client
                                   -- sending request block until argument
                                   -- values become available
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  Intern
    :: SymbolName
    -> out (RawValue 'Unknown)
    -> EmacsCall f out (RawValue 'Unknown)

  TypeOf
    :: RawValue p
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  IsNotNil
    :: RawValue p
    -> out Bool
    -> EmacsCall f out Bool

  Eq
    :: RawValue p1
    -> RawValue p2
    -> out Bool
    -> EmacsCall f out Bool

  ExtractInteger
    :: RawValue p
    -> out (f EmacsSignal Void Int64)
    -> EmacsCall f out (f EmacsSignal Void Int64)
  MakeInteger
    :: !Int64
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  ExtractFloat
    :: RawValue p
    -> out (f EmacsSignal Void Double)
    -> EmacsCall f out (f EmacsSignal Void Double)
  MakeFloat
    :: !Double
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  ExtractString
    :: RawValue p
    -> out (f EmacsSignal Void BS.ByteString)
    -> EmacsCall f out (f EmacsSignal Void BS.ByteString)
  MakeString
    :: !BS.ByteString
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  GetUserPtr
    :: RawValue p
    -> out (f EmacsSignal Void (Ptr a))
    -> EmacsCall f out (f EmacsSignal Void (Ptr a))
  MakeUserPtr
    :: !(FinalizerPtr a)
    -> !(Ptr a)
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)
  SetUserPtr
    :: RawValue 'Regular
    -> !(Ptr a)
    -> out (f EmacsSignal Void ())
    -> EmacsCall f out (f EmacsSignal Void ())

  GetUserPtrFinaliser
    :: RawValue p
    -> out (f EmacsSignal Void (FinalizerPtr a))
    -> EmacsCall f out (f EmacsSignal Void (FinalizerPtr a))
  SetUserPtrFinaliser
    :: RawValue p
    -> !(FinalizerPtr a)
    -> out (f EmacsSignal Void ())
    -> EmacsCall f out (f EmacsSignal Void ())

  VecGet
    :: RawValue p
    -> !Int
    -> out (f EmacsSignal Void (RawValue 'Regular))
    -> EmacsCall f out (f EmacsSignal Void (RawValue 'Regular))
  VecSet
    :: RawValue p1
    -> !Int
    -> RawValue p2
    -> out (f EmacsSignal Void ())
    -> EmacsCall f out (f EmacsSignal Void ())
  VecSize
    :: RawValue p
    -> out (f EmacsSignal Void Int)
    -> EmacsCall f out (f EmacsSignal Void Int)

  ProcessInput
    :: EmacsCall f out ()

deriving instance (forall x. Show (out x)) => Show (EmacsCall f out a)
