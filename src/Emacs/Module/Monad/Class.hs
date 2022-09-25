----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Class
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Emacs.Module.Monad.Class
  ( EmacsFunction
  , MonadEmacs(..)
  , MakeEmacsRef(..)
  ) where

import Control.Exception.Safe.Checked (Throws)
import Data.ByteString qualified as BS
import Data.Int
import Data.Kind
import Foreign.Ptr (Ptr)
import Prettyprinter

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.Env (UserPtrFinaliser)
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.GetRawValue
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName
import Emacs.Module.Assert
import Emacs.Module.Errors

-- | Basic Haskell function that can be called by Emacs.
type EmacsFunction req opt rest (s :: k) (m :: k -> Type -> Type)
  = EmacsArgs req opt rest (EmacsRef m s) -> m s (EmacsRef m s)

-- | Type class for injecting things into 'EmacsRef' values that can be managed my monad @m@.
class MakeEmacsRef a (m :: k -> Type -> Type) where
  makeEmacsRef
    :: WithCallStack
    => a
    -> m s (EmacsRef m s)

-- | A mtl-style typeclass for interacting with Emacs. Typeclass functions
-- are mostly direct translations of emacs interface provided by 'emacs-module.h'.
--
-- For more functions please refer to "Emacs.Module.Functions" module.
class (forall s. Monad (m s), MakeEmacsRef RawValue m, MakeEmacsRef GlobalRef m) => MonadEmacs (m :: k -> Type -> Type) where

  -- | Emacs value that is managed by the 'm' monad. Will be cleaned up
  -- after 'm' finishes its execution.
  type EmacsRef m :: k -> Type

  -- | Check whether a non-local exit is pending.
  nonLocalExitCheck :: WithCallStack => m s (FuncallExit ())

  -- | Check whether a non-local exit is pending and get detailed data
  -- in case it is.
  nonLocalExitGet
    :: WithCallStack
    => m s (FuncallExit (EmacsRef m s, EmacsRef m s))

  -- | Equivalent to Emacs's @signal@ function.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, use 'nonLocalExitClear'.
  nonLocalExitSignal
    :: WithCallStack
    => EmacsRef m s   -- ^ Error symbol
    -> [EmacsRef m s] -- ^ Error data, will be converted to a list as Emacs API expects.
    -> m s ()

  -- | Equivalent to Emacs's @throw@ function.
  --
  -- NB if a non-local exit is alredy pending, this function will not
  -- overwrite it. In order to do that, use 'nonLocalExitClear'.
  nonLocalExitThrow
    :: WithCallStack
    => EmacsRef m s -- ^ Tag
    -> EmacsRef m s -- ^ Data
    -> m s ()

  -- | Clean any pending local exits.
  nonLocalExitClear :: WithCallStack => m s ()

  -- | Make Haskell function available as an anonymous Emacs
  -- function. In order to be able to use it later from Emacs it should
  -- be fed into 'bindFunction'.
  makeFunction
    :: (WithCallStack, EmacsInvocation req opt rest, GetArities req opt rest)
    => (forall s'.
         (Throws EmacsInternalError, Throws EmacsError, Throws EmacsThrow, Throws UserError) =>
         EmacsFunction req opt rest s' m) -- ^ Haskell function to export
    -> Doc.Doc                                      -- ^ Documentation
    -> m s (EmacsRef m s)

  -- | Invoke an Emacs function that may call back into Haskell.
  funcall
    :: (WithCallStack, Pretty a, UseSymbolName a, GetRawValue (ReifiedSymbol a))
    => SymbolName a    -- ^ Function name
    -> [EmacsRef m s]  -- ^ Arguments
    -> m s (EmacsRef m s)

  -- | Invoke an Emacs function. The function should be simple and
  -- must not call back into Haskell.
  funcallPrimitive
    :: (WithCallStack, Pretty a, UseSymbolName a)
    => SymbolName a    -- ^ Function name
    -> [EmacsRef m s]  -- ^ Arguments
    -> m s (EmacsRef m s)

  -- | Invoke an Emacs function and ignore its result. The function
  -- should be simple and must not call back into Haskell.
  funcallPrimitive_
    :: (WithCallStack, Pretty a, UseSymbolName a)
    => SymbolName a   -- ^ Function name
    -> [EmacsRef m s] -- ^ Arguments
    -> m s ()

  -- | Convert a string to an Emacs symbol.
  intern
    :: (WithCallStack, Pretty a, UseSymbolName a, MakeEmacsRef (ReifiedSymbol a) m)
    => SymbolName a
    -> m s (EmacsRef m s)

  -- | Get type of an Emacs value as an Emacs symbol.
  typeOf
    :: WithCallStack
    => EmacsRef m s -> m s (EmacsRef m s)

  -- | Check whether Emacs value is not @nil@.
  isNotNil :: WithCallStack => EmacsRef m s -> m s Bool

  -- | Primitive equality. Tests whether two symbols, integers or
  -- characters are the equal, but not much more. For more complete
  -- equality comparison do
  --
  -- > funcallPrimitive [esym|equal|] [x, y]
  eq
    :: WithCallStack
    => EmacsRef m s -> EmacsRef m s -> m s Bool


  -- | Try to unpack a wide integer from a value.
  extractWideInteger :: WithCallStack => EmacsRef m s -> m s Int64

  -- | Pack a wide integer for Emacs.
  makeWideInteger :: WithCallStack => Int64 -> m s (EmacsRef m s)

  -- | Try to unpack a floating-point number from a value.
  extractDouble :: WithCallStack => EmacsRef m s -> m s Double

  -- | Convert a floating-point number into Emacs value.
  makeDouble :: WithCallStack => Double -> m s (EmacsRef m s)

  -- | Extract string contents from an Emacs value.
  extractString :: WithCallStack => EmacsRef m s -> m s BS.ByteString

  -- | Convert a utf8-encoded ByteString into an Emacs value.
  makeString :: WithCallStack => BS.ByteString -> m s (EmacsRef m s)


  -- | Extract a user pointer from an Emacs value.
  extractUserPtr :: WithCallStack => EmacsRef m s -> m s (Ptr a)

  -- | Pack a user pointer into an Emacs value.
  makeUserPtr
    :: WithCallStack
    => UserPtrFinaliser a -- ^ Finalisation action that will be executed when user pointer gets garbage-collected by Emacs.
    -> Ptr a
    -> m s (EmacsRef m s)

  -- | Set user pointer to a new value
  assignUserPtr :: WithCallStack => EmacsRef m s -> Ptr a -> m s ()

  -- | Extract a finaliser from an user_ptr.
  extractUserPtrFinaliser
    :: WithCallStack => EmacsRef m s -> m s (UserPtrFinaliser a)

  -- | Assign new finaliser into an user_ptr.
  assignUserPtrFinaliser
    :: WithCallStack => EmacsRef m s -> UserPtrFinaliser a -> m s ()

  -- | Extract an element from an Emacs vector.
  vecGet :: WithCallStack => EmacsRef m s -> Int -> m s (EmacsRef m s)

  -- | Assign an element into an Emacs vector.
  vecSet
    :: WithCallStack
    => EmacsRef m s -- ^ Vector
    -> Int          -- ^ Index
    -> EmacsRef m s -- ^ New value
    -> m s ()

  -- | Get size of an Emacs vector.
  vecSize :: WithCallStack => EmacsRef m s -> m s Int
