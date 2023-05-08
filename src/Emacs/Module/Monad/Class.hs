----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Class
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE TypeFamilies           #-}

module Emacs.Module.Monad.Class
  ( EmacsFunction
  , MonadEmacs(..)
  ) where

import Control.Monad.Interleave
import Control.Monad.Primitive
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Int
import Data.Kind
import Data.Text (Text)
import Data.Vector.Unboxed qualified as U
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Ptr (Ptr)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.Env.ProcessInput qualified as ProcessInput
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName
import Emacs.Module.Assert

-- | Basic Haskell function that can be called by Emacs.
type EmacsFunction req opt rest (m :: k -> Type -> Type) (v :: k -> Type) (s :: k)
  = EmacsArgs req opt rest (v s) -> m s (v s)

-- | A mtl-style typeclass for interacting with Emacs. Typeclass functions
-- are mostly direct translations of emacs interface provided by 'emacs-module.h'.
--
-- For more functions please refer to "Emacs.Module.Functions" module.
class
  ( forall s. Monad (m s)
  , forall s. MonadInterleave (m s)
  , forall s. U.Unbox (v s)
  , forall s. PrimMonad (m s)
  ) => MonadEmacs (m :: k -> Type -> Type) (v :: k -> Type) | m -> v where

  -- -- | Emacs value that is managed by the 'm' monad. Will be cleaned up
  -- -- after 'm' finishes its execution.
  -- type EmacsRef m :: k -> Type

  -- | Make a global reference to a value so that it will persist
  -- across different calls from Emacs into exposed functions.
  makeGlobalRef :: WithCallStack => v s -> m s (RawValue 'Pinned)

  -- | Free a global reference.
  freeGlobalRef :: WithCallStack => RawValue 'Pinned -> m s ()

  -- | Check whether a non-local exit is pending.
  nonLocalExitCheck :: WithCallStack => m s (FuncallExit ())

  -- | Check whether a non-local exit is pending and get detailed data
  -- in case it is.
  nonLocalExitGet
    :: WithCallStack
    => m s (FuncallExit (v s, v s))

  -- | Equivalent to Emacs's @signal@ function. Terminates current computation.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, first use 'nonLocalExitClear'.
  nonLocalExitSignal
    :: (WithCallStack, Foldable f)
    => v s     -- ^ Error symbol
    -> f (v s) -- ^ Error data, will be converted to a list as Emacs API expects.
    -> m s ()

  -- | Equivalent to Emacs's @throw@ function. Terminates current computation.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, use 'nonLocalExitClear'.
  nonLocalExitThrow
    :: WithCallStack
    => v s -- ^ Tag
    -> v s -- ^ Data
    -> m s ()

  -- | Clean any pending local exits.
  nonLocalExitClear :: WithCallStack => m s ()

  -- | Make Haskell function available as an anonymous Emacs
  -- function. In order to be able to use it later from Emacs it should
  -- be fed into 'bindFunction'.
  makeFunction
    :: (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => (forall s'. EmacsFunction req opt rest m v s') -- ^ Haskell function to export
    -> Doc.Doc                                        -- ^ Documentation
    -> m s (v s)

  -- | Invoke an Emacs function that may call back into Haskell.
  funcall
    :: (WithCallStack, Foldable f)
    => v s     -- ^ Function name
    -> f (v s) -- ^ Arguments
    -> m s (v s)

  -- | Invoke an Emacs function. The function should be simple and
  -- must not call back into Haskell.
  funcallPrimitive
    :: (WithCallStack, Foldable f)
    => v s     -- ^ Function name
    -> f (v s) -- ^ Arguments
    -> m s (v s)

  -- | Invoke an Emacs function. The function should be simple and
  -- must not call back into Haskell.
  --
  -- Exit status is not checked - function is expected to always
  -- succeed. Consult Emacs side to make sure that's the case.
  -- Examples of safe functions: @cons@, @list@, @vector@, etc.
  funcallPrimitiveUnchecked
    :: (WithCallStack, Foldable f)
    => v s     -- ^ Function name
    -> f (v s) -- ^ Arguments
    -> m s (v s)

  -- | Convert a string to an Emacs symbol.
  intern
    :: WithCallStack
    => SymbolName
    -> m s (v s)

  -- | Get type of an Emacs value as an Emacs symbol.
  typeOf
    :: WithCallStack
    => v s -> m s (v s)

  -- | Check whether Emacs value is not @nil@.
  isNotNil :: WithCallStack => v s -> m s Bool

  -- | Primitive equality. Tests whether two symbols, integers or
  -- characters are the equal, but not much more. For more complete
  -- equality comparison do
  --
  -- > intern "equal" >>= \equal -> funcallPrimitiveUnchecked equal [x, y]
  eq
    :: WithCallStack
    => v s -> v s -> m s Bool


  -- | Try to unpack a wide integer from a value.
  extractWideInteger :: WithCallStack => v s -> m s Int64

  -- | Pack a wide integer for Emacs.
  makeWideInteger :: WithCallStack => Int64 -> m s (v s)

  -- | Try to unpack a floating-point number from a value.
  extractDouble :: WithCallStack => v s -> m s Double

  -- | Convert a floating-point number into Emacs value.
  makeDouble :: WithCallStack => Double -> m s (v s)

  -- | Extract string contents from an Emacs value.
  extractText :: WithCallStack => v s -> m s Text

  -- | Extract string contents from an Emacs value as utf8-encoded short bytestring.
  extractShortByteString :: WithCallStack => v s -> m s BSS.ShortByteString

  -- | Convert a utf8-encoded ByteString into an Emacs value.
  makeString :: WithCallStack => BS.ByteString -> m s (v s)


  -- | Extract a user pointer from an Emacs value.
  extractUserPtr :: WithCallStack => v s -> m s (Ptr a)

  -- | Pack a user pointer into an Emacs value.
  makeUserPtr
    :: WithCallStack
    => FinalizerPtr a -- ^ Finalisation action that will be executed when user pointer gets garbage-collected by Emacs.
    -> Ptr a
    -> m s (v s)

  -- | Set user pointer to a new value
  assignUserPtr :: WithCallStack => v s -> Ptr a -> m s ()

  -- | Extract a finaliser from an user_ptr.
  extractUserPtrFinaliser
    :: WithCallStack => v s -> m s (FinalizerPtr a)

  -- | Assign new finaliser into an user_ptr.
  assignUserPtrFinaliser
    :: WithCallStack => v s -> FinalizerPtr a -> m s ()

  -- | Extract an element from an Emacs vector.
  vecGet :: WithCallStack => v s -> Int -> m s (v s)

  -- | Extract an element from an Emacs vector without checking for errors.
  unsafeVecGet :: WithCallStack => v s -> Int -> m s (v s)

  -- | Assign an element into an Emacs vector.
  vecSet
    :: WithCallStack
    => v s -- ^ Vector
    -> Int -- ^ Index
    -> v s -- ^ New value
    -> m s ()

  -- | Get size of an Emacs vector.
  vecSize :: WithCallStack => v s -> m s Int

  -- | Check whether user pressed 'C-g' and we should abort our operation.
  processInput :: WithCallStack => m s ProcessInput.Result
