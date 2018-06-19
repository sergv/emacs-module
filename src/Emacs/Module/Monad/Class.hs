----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Class
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Emacs.Module.Monad.Class
  ( EmacsFunction
  , EmacsFunctionExtra
  , MonadEmacs(..)
  ) where

import Control.Exception.Safe.Checked (Throws)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Int
import Foreign.Ptr (Ptr)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Env (UserPtrFinaliser)
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName (SymbolName)
import Data.Emacs.Module.Value.Internal
import Emacs.Module.Assert
import Emacs.Module.Errors

type EmacsFunction req opt rest s (m :: * -> * -> *)
  = (Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError, Throws UserError)
  => EmacsArgs req opt rest (Value s) -> m s (Value s)

type EmacsFunctionExtra req opt rest extra s (m :: * -> * -> *)
  = (Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError, Throws UserError)
  => EmacsArgs req opt rest (Value s) -> Ptr extra -> m s (Value s)

class MonadEmacs (m :: * -> * -> *) where

  -- | Check whether a non-local exit is pending.
  nonLocalExitCheck :: WithCallStack => m s (FuncallExit ())

  -- | Check whether a non-local exit is pending and get detailed data
  -- in case it is.
  nonLocalExitGet
    :: WithCallStack
    => m s (FuncallExit (Value s, Value s))

  -- | Equivalent to Emacs's @signal@ function.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, use nonLocalExitClear.
  nonLocalExitSignal
    :: WithCallStack
    => Value s   -- ^ Error symbol
    -> [Value s] -- ^ Error data, will be converted to a list as Emacs API expects.
    -> m s ()

  -- | Equivalent to Emacs's @throw@ function.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, use nonLocalExitClear.
  nonLocalExitThrow
    :: WithCallStack
    => Value s -- ^ Tag
    -> Value s -- ^ Data
    -> m s ()

  -- | Clean any pending local exits.
  nonLocalExitClear :: WithCallStack => m s ()


  -- | Protect a raw value (i.e. a plain pointer) from Emacs GC.
  --
  -- Users writing emacs extersions will likely have no need to
  -- call this function directly.
  makeValue :: WithCallStack => RawValue -> m s (Value s)

  -- | Make value eligible for collection during next GC within Emacs.
  freeValue :: WithCallStack => Value s -> m s ()

  -- | Make Haskell function available as an anonymoucs Emacs
  -- function. In order to be able to use it later from Emacs it should
  -- be fed into 'bindFunction'.
  --
  -- NB Each call to this function produces a small memory leak that
  -- will not be freed up. Hence, try not to create unbounded number
  -- of functions. This happens because GHC has to generate some wrapping
  -- code to convert between ccall and Haskell calling convention each time
  -- a function is exported. It is possible to free this code after function
  -- will not be used, but it's currently not supported.
  makeFunctionExtra
    :: (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => (forall s'. EmacsFunctionExtra req opt rest extra s' m) -- ^ Haskell function to export
    -> C8.ByteString                                           -- ^ Documentation
    -> Ptr extra                                               -- ^ Extra data to be passed to the Haskell function
    -> m s (Value s)

  -- | Invoke an Emacs function that may call back into Haskell.
  funcall
    :: WithCallStack
    => SymbolName -- ^ Function name
    -> [Value s]  -- ^ Arguments
    -> m s (Value s)

  -- | Invoke an Emacs function. The function should be simple and
  -- must not call back into Haskell.
  funcallPrimitive
    :: WithCallStack
    => SymbolName -- ^ Function name
    -> [Value s]  -- ^ Arguments
    -> m s (Value s)

  -- | Convert a string to an Emacs symbol.
  intern
    :: WithCallStack
    => SymbolName
    -> m s (Value s)

  -- | Get type of an Emacs value as an Emacs symbol.
  typeOf
    :: WithCallStack
    => Value s -> m s (Value s)

  -- | Check whether Emacs value is not @nil@.
  isNotNil :: WithCallStack => Value s -> m s Bool

  -- | Primitive equality. Tests whether two symbols, integers or
  -- characters are the equal, but not much more. For more complete
  -- equality comparison do
  --
  -- > funcall [esym|equal|] [x, y]
  eq
    :: WithCallStack
    => Value s -> Value s -> m s Bool


  -- | Try to unpack a wide integer from a value.
  extractWideInteger :: WithCallStack => Value s -> m s Int64

  -- | Pack a wide integer for Emacs.
  makeWideInteger :: WithCallStack => Int64 -> m s (Value s)

  -- | Try to unpack a floating-point number from a value.
  extractDouble :: WithCallStack => Value s -> m s Double

  -- | Convert a floating-point number into Emacs value.
  makeDouble :: WithCallStack => Double -> m s (Value s)

  -- | Extract string contents from an Emacs value.
  extractString :: WithCallStack => Value s -> m s BS.ByteString

  -- | Convert a utf8-encoded ByteString into an Emacs value.
  makeString :: WithCallStack => BS.ByteString -> m s (Value s)


  -- | Extract a user pointer from an Emacs value.
  extractUserPtr :: WithCallStack => Value s -> m s (Ptr a)

  -- | Pack a user pointer into an Emacs value.
  makeUserPtr
    :: WithCallStack
    => UserPtrFinaliser a -- ^ Finalisation action that will be executed when user pointer gets garbage-collected by Emacs.
    -> Ptr a
    -> m s (Value s)

  -- | Set user pointer to a new value
  assignUserPtr :: WithCallStack => Value s -> Ptr a -> m s ()

  -- | Extract a finaliser from an user_ptr.
  extractUserPtrFinaliser
    :: WithCallStack => Value s -> m s (UserPtrFinaliser a)

  -- | Assign new finaliser into an user_ptr.
  assignUserPtrFinaliser
    :: WithCallStack => Value s -> UserPtrFinaliser a -> m s ()

  -- | Extract an element from an Emacs vector.
  vecGet :: WithCallStack => Value s -> Int -> m s (Value s)

  -- | Assign an element into an Emacs vector.
  vecSet
    :: WithCallStack
    => Value s -- ^ Vector
    -> Int     -- ^ Index
    -> Value s -- ^ New value
    -> m s ()

  -- | Get size of an Emacs vector.
  vecSize :: WithCallStack => Value s -> m s Int
