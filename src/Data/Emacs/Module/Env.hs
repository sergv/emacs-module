----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Emacs.Module.Env
  ( Env

  , -- * enum emacs_funcall_exit
    FuncallExit(..)
  , funcallExitToNum
  , funcallExitFromNum

  , -- * Wrappers around struct emacs_env fields
    EnumFuncallExit(..)
  , UserPtrFinaliserType
  , UserPtrFinaliser
  , isValidEnv

  , makeGlobalRef
  , freeGlobalRef

  , nonLocalExitCheck
  , nonLocalExitGet
  , nonLocalExitSignal
  , nonLocalExitThrow
  , nonLocalExitClear

  , variadicFunctionArgs
  , makeFunction

  , funcall
  , funcallPrimitive
  , intern
  , typeOf
  , isNotNil
  , eq
  , extractInteger
  , makeInteger
  , extractFloat
  , makeFloat
  , copyStringContents
  , makeString
  , makeUserPtr
  , getUserPtr
  , setUserPtr
  , getUserFinaliser
  , setUserFinaliser
  , vecGet
  , vecSet
  , vecSize

  , -- * Expose functions to Emacs
    exportToEmacs
  , RawFunctionType
  , RawFunction

    -- * Expose Haskell data to Emacs
  , freeStablePtrFinaliser
  ) where

import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Env

-- | Pass to 'makeUserPtr' so that Emacs will free the Haskell's stable
-- pointer when the corresponding elisp value goes out of scope.
foreign import ccall "& hs_free_stable_ptr" freeStablePtrFinaliser :: UserPtrFinaliser a
