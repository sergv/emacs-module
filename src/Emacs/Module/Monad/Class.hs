----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Class
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

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
import Data.Emacs.Module.Raw.Env.Internal (Env)
import Data.Emacs.Module.SymbolName (SymbolName)
import qualified Data.Emacs.Module.Value as Emacs
import Emacs.Module.Assert
import Emacs.Module.Errors

type EmacsFunction req opt rest
  = (Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError, Throws UserError)
  => Env -> EmacsArgs req opt rest Emacs.RawValue

type EmacsFunctionExtra extra req opt rest
  = (Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError, Throws UserError)
  => Env -> Ptr extra -> EmacsArgs req opt rest Emacs.RawValue

class Monad m => MonadEmacs m where

  -- | Check whether a non-local exit is pending.
  nonLocalExitCheck :: WithCallStack => m (FuncallExit ())

  -- | Check whether a non-local exit is pending and get detailed data
  -- in case it is.
  nonLocalExitGet
    :: WithCallStack
    => m (FuncallExit (Emacs.RawValue, Emacs.RawValue))

  -- | Equivalent to Emacs's @signal@ function.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, use nonLocalExitClear.
  nonLocalExitSignal
    :: WithCallStack
    => Emacs.RawValue   -- ^ Error symbol
    -> [Emacs.RawValue] -- ^ Error data, will be converted to a list as Emacs API expects.
    -> m ()

  -- | Equivalent to Emacs's @throw@ function.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, use nonLocalExitClear.
  nonLocalExitThrow
    :: WithCallStack
    => Emacs.RawValue -- ^ Tag
    -> Emacs.RawValue -- ^ Data
    -> m ()

  -- | Clean any pending local exits.
  nonLocalExitClear :: WithCallStack => m ()


  -- | Make a global reference to a value so that it will persist
  -- across different calls from Emacs into exposed functions.
  makeGlobalRef :: WithCallStack => Emacs.RawValue -> m Emacs.GlobalRef

  -- | Free a global reference.
  freeGlobalRef :: WithCallStack => Emacs.GlobalRef -> m ()


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
    => EmacsFunctionExtra extra req opt rest -- ^ Haskell function to export
    -> C8.ByteString                         -- ^ Documentation
    -> Ptr extra                             -- ^ Extra data to be passed to the Haskell function
    -> m Emacs.RawValue


  -- | Invoke an Emacs function that may call back into Haskell.
  funcall
    :: WithCallStack
    => SymbolName    -- ^ Function name
    -> [Emacs.RawValue] -- ^ Arguments
    -> m Emacs.RawValue

  -- | Invoke an Emacs function. The function should be simple and
  -- must not call back into Haskell.
  funcallPrimitive
    :: WithCallStack
    => SymbolName    -- ^ Function name
    -> [Emacs.RawValue] -- ^ Arguments
    -> m Emacs.RawValue

  -- | Convert a string to an Emacs symbol.
  intern
    :: WithCallStack
    => SymbolName
    -> m Emacs.RawValue

  -- | Get type of an Emacs value as an Emacs symbol.
  typeOf
    :: WithCallStack
    => Emacs.RawValue -> m Emacs.RawValue

  -- | Check whether Emacs value is not @nil@.
  isNotNil :: WithCallStack => Emacs.RawValue -> m Bool

  -- | Primitive equality. Tests whether two symbols, integers or
  -- characters are the equal, but not much more. For more complete
  -- equality comparison do
  --
  -- > funcall [esym|equal|] [x, y]
  eq
    :: WithCallStack
    => Emacs.RawValue -> Emacs.RawValue -> m Bool


  -- | Try to unpack a wide integer from a value.
  extractWideInteger :: WithCallStack => Emacs.RawValue -> m Int64

  -- | Pack a wide integer for Emacs.
  makeWideInteger :: WithCallStack => Int64 -> m Emacs.RawValue

  -- | Try to unpack a floating-point number from a value.
  extractDouble :: WithCallStack => Emacs.RawValue -> m Double

  -- | Convert a floating-point number into Emacs value.
  makeDouble :: WithCallStack => Double -> m Emacs.RawValue

  -- | Extract string contents from an Emacs value.
  extractString :: WithCallStack => Emacs.RawValue -> m BS.ByteString

  -- | Convert a utf8-encoded ByteString into an Emacs value.
  makeString :: WithCallStack => BS.ByteString -> m Emacs.RawValue


  -- | Extract a user pointer from an Emacs value.
  extractUserPtr :: WithCallStack => Emacs.RawValue -> m (Ptr a)

  -- | Pack a user pointer into an Emacs value.
  makeUserPtr
    :: WithCallStack
    => UserPtrFinaliser a -- ^ Finalisation action that will be executed when user pointer gets garbage-collected by Emacs.
    -> Ptr a
    -> m Emacs.RawValue

  -- | Set user pointer to a new value
  assignUserPtr :: WithCallStack => Emacs.RawValue -> Ptr a -> m ()

  -- | Extract a finaliser from an user_ptr.
  extractUserPtrFinaliser
    :: WithCallStack => Emacs.RawValue -> m (UserPtrFinaliser a)

  -- | Assign new finaliser into an user_ptr.
  assignUserPtrFinaliser
    :: WithCallStack => Emacs.RawValue -> UserPtrFinaliser a -> m ()

  -- | Extract an element from an Emacs vector.
  vecGet :: WithCallStack => Emacs.RawValue -> Int -> m Emacs.RawValue

  -- | Assign an element into an Emacs vector.
  vecSet
    :: WithCallStack
    => Emacs.RawValue -- ^ Vector
    -> Int         -- ^ Index
    -> Emacs.RawValue -- ^ New value
    -> m ()

  -- | Get size of an Emacs vector.
  vecSize :: WithCallStack => Emacs.RawValue -> m Int
