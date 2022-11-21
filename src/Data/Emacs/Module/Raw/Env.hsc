----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Env
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Low-level and, hopefully, low-overhead wrappers around @struct emacs_env@.
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UnliftedFFITypes           #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.Raw.Env
  ( EnumFuncallExit(..)
  , EnumProcessInputResult(..)
  , Env
  , CBoolean
  , isTruthy
  , isNonTruthy
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
  , setFunctionFinalizer
  , processInput
  ) where

import Control.Monad.IO.Class

import Data.Coerce
import Foreign
import Foreign.C
import Prettyprinter

import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env.Internal as Env
import Data.Emacs.Module.Raw.Env.TH
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.Raw.Value.Internal

import Data.Emacs.Module.NonNullPtr.Internal

#include <emacs-module.h>

newtype EnumFuncallExit = EnumFuncallExit { unEnumFuncallExit :: CInt }

instance Pretty EnumFuncallExit where
  pretty (EnumFuncallExit (CInt x)) = pretty x

newtype EnumProcessInputResult = EnumProcessInputResult { unEnumProcessInputResult :: CInt }

instance Pretty EnumProcessInputResult where
  pretty (EnumProcessInputResult (CInt x)) = pretty x

-- | A wrapper around C value that denotes true or false.
newtype CBoolean = CBoolean (#type bool)

{-# INLINE isTruthy #-}
-- | Check whether a 'CBoolean' denotes true.
isTruthy :: CBoolean -> Bool
isTruthy (CBoolean a) = a /= 0

{-# INLINE isNonTruthy #-}
-- | Check whether a 'CBoolean' denotes false.
isNonTruthy :: CBoolean -> Bool
isNonTruthy (CBoolean a) = a == 0



{-# INLINE isValidEnv #-}
-- | Check wheter passed @emacs_env@ structure has expected size so that
-- we will be able to access all of its fields.
isValidEnv :: MonadIO m => Env -> m Bool
isValidEnv env = liftIO $ do
  realSize <- (#peek emacs_env, size) (Env.toPtr env)
  pure $ expectedSize <= realSize
  where
    expectedSize :: CPtrdiff
    expectedSize = (#size struct emacs_env_28)

$(wrapEmacsFunc "makeGlobalRefTH" Unsafe
   [e| (#peek emacs_env, make_global_ref) |]
   [t| forall p. Env -> RawValue p -> IO (RawValue 'Pinned) |])

{-# INLINE makeGlobalRef #-}
makeGlobalRef
  :: forall m p. MonadIO m
  => Env
  -> RawValue p
  -> m (RawValue 'Pinned)
makeGlobalRef env x =
  liftIO $ makeGlobalRefTH env x


$(wrapEmacsFunc "freeGlobalRefTH" Unsafe
   [e| (#peek emacs_env, free_global_ref) |]
   [t| Env -> RawValue 'Pinned -> IO () |])

{-# INLINE freeGlobalRef #-}
freeGlobalRef
  :: forall m. MonadIO m
  => Env
  -> RawValue 'Pinned
  -> m ()
freeGlobalRef env x =
  liftIO $ freeGlobalRefTH env x


$(wrapEmacsFunc "nonLocalExitCheckTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_check) |]
   [t| Env -> IO EnumFuncallExit |])

{-# INLINE nonLocalExitCheck #-}
nonLocalExitCheck
  :: MonadIO m
  => Env
  -> m EnumFuncallExit
nonLocalExitCheck = nonLocalExitCheckTH


$(wrapEmacsFunc "nonLocalExitGetTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_get) |]
   [t| Env -> NonNullPtr (RawValue 'Regular) -> NonNullPtr (RawValue 'Regular) -> IO EnumFuncallExit |])

{-# INLINE nonLocalExitGet #-}
nonLocalExitGet
  :: MonadIO m
  => Env
  -> NonNullPtr (RawValue 'Regular) -- ^ Symbol output
  -> NonNullPtr (RawValue 'Regular) -- ^ Data output
  -> m EnumFuncallExit
nonLocalExitGet = nonLocalExitGetTH


$(wrapEmacsFunc "nonLocalExitSignalTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_signal) |]
   [t| forall p1 p2. Env -> RawValue p1 -> RawValue p2 -> IO () |])

{-# INLINE nonLocalExitSignal #-}
nonLocalExitSignal
  :: MonadIO m
  => Env
  -> RawValue p1 -- ^ Error symbol
  -> RawValue p2 -- ^ Error data
  -> m ()
nonLocalExitSignal = nonLocalExitSignalTH


$(wrapEmacsFunc "nonLocalExitThrowTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_throw) |]
   [t| forall p1 p2. Env -> RawValue p1 -> RawValue p2 -> IO () |])

{-# INLINE nonLocalExitThrow #-}
nonLocalExitThrow
  :: MonadIO m
  => Env
  -> RawValue p1 -- ^ Tag, a symbol
  -> RawValue p2 -- ^ Value
  -> m ()
nonLocalExitThrow = nonLocalExitThrowTH


$(wrapEmacsFunc "nonLocalExitClearTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_clear) |]
   [t| Env -> IO () |])

{-# INLINE nonLocalExitClear #-}
nonLocalExitClear
  :: MonadIO m
  => Env
  -> m ()
nonLocalExitClear = nonLocalExitClearTH


variadicFunctionArgs :: CPtrdiff
variadicFunctionArgs = (#const emacs_variadic_function)

$(wrapEmacsFunc "makeFunctionTH" Unsafe
   [e| (#peek emacs_env, make_function) |]
   [t| forall a o. Env -> CPtrdiff -> CPtrdiff -> FunPtr (RawFunctionType o a) -> CString -> Ptr a -> IO (RawValue 'Regular) |])

{-# INLINE makeFunction #-}
makeFunction
  :: forall m o a. MonadIO m
  => Env
  -> CPtrdiff        -- ^ Minimum arity
  -> CPtrdiff        -- ^ Maximum arity
  -> RawFunction o a -- ^ Implementation
  -> CString         -- ^ Documentation
  -> Ptr a           -- ^ Extra data
  -> m (RawValue 'Regular)
makeFunction =
  coerce
    (makeFunctionTH ::
         Env
      -> CPtrdiff
      -> CPtrdiff
      -> FunPtr (RawFunctionType o a)
      -> CString
      -> Ptr a
      -> m (RawValue 'Regular))


$(wrapEmacsFunc "funcallTH" Safe
   [e| (#peek emacs_env, funcall) |]
   [t| forall p1 p2. Env -> RawValue p1 -> CPtrdiff -> NonNullPtr (RawValue p2) -> IO (RawValue 'Regular) |])

{-# INLINE funcall #-}
funcall
  :: MonadIO m
  => Env
  -> RawValue p1              -- ^ Function
  -> CPtrdiff                 -- ^ Number of arguments
  -> NonNullPtr (RawValue p2) -- ^ Actual arguments
  -> m (RawValue 'Regular)
funcall = funcallTH


$(wrapEmacsFunc "funcallPrimitiveTH" Unsafe
   [e| (#peek emacs_env, funcall) |]
   [t| forall p1 p2. Env -> RawValue p1 -> CPtrdiff -> NonNullPtr (RawValue p2) -> IO (RawValue 'Regular) |])

{-# INLINE funcallPrimitive #-}
funcallPrimitive
  :: MonadIO m
  => Env
  -> RawValue p1              -- ^ Function
  -> CPtrdiff                 -- ^ Number of arguments
  -> NonNullPtr (RawValue p2) -- ^ Actual arguments
  -> m (RawValue 'Regular)
funcallPrimitive = funcallPrimitiveTH


$(wrapEmacsFunc "internTH" Unsafe
   [e| (#peek emacs_env, intern) |]
   [t| Env -> CString -> IO (RawValue 'Regular) |])

{-# INLINE intern #-}
intern
  :: MonadIO m
  => Env
  -> CString
  -> m (RawValue 'Regular)
intern = internTH


$(wrapEmacsFunc "typeOfTH" Unsafe
   [e| (#peek emacs_env, type_of) |]
   [t| forall p. Env -> RawValue p -> IO (RawValue 'Regular) |])

{-# INLINE typeOf #-}
typeOf
  :: MonadIO m
  => Env
  -> RawValue p
  -> m (RawValue 'Regular)
typeOf = typeOfTH


$(wrapEmacsFunc "isNotNilTH" Unsafe
   [e| (#peek emacs_env, is_not_nil) |]
   [t| forall p. Env -> RawValue p -> IO CBoolean |])

{-# INLINE isNotNil #-}
isNotNil
  :: MonadIO m
  => Env
  -> RawValue p
  -> m CBoolean
isNotNil = isNotNilTH


$(wrapEmacsFunc "eqTH" Unsafe
   [e| (#peek emacs_env, eq) |]
   [t| forall p1 p2. Env -> RawValue p1 -> RawValue p2 -> IO CBoolean |])

{-# INLINE eq #-}
eq
  :: MonadIO m
  => Env
  -> RawValue p1
  -> RawValue p2
  -> m CBoolean
eq = eqTH


$(wrapEmacsFunc "extractIntegerTH" Unsafe
   [e| (#peek emacs_env, extract_integer) |]
   [t| forall p. Env -> RawValue p -> IO CIntMax |])

{-# INLINE extractInteger #-}
extractInteger
  :: MonadIO m
  => Env
  -> RawValue p
  -> m CIntMax
extractInteger = extractIntegerTH


$(wrapEmacsFunc "makeIntegerTH" Unsafe
   [e| (#peek emacs_env, make_integer) |]
   [t| Env -> CIntMax -> IO (RawValue 'Regular) |])

{-# INLINE makeInteger #-}
makeInteger
  :: MonadIO m
  => Env
  -> CIntMax
  -> m (RawValue 'Regular)
makeInteger = makeIntegerTH


$(wrapEmacsFunc "extractFloatTH" Unsafe
   [e| (#peek emacs_env, extract_float) |]
   [t| forall p. Env -> RawValue p -> IO CDouble |])

{-# INLINE extractFloat #-}
extractFloat
  :: MonadIO m
  => Env
  -> RawValue p
  -> m CDouble
extractFloat = extractFloatTH


$(wrapEmacsFunc "makeFloatTH" Unsafe
   [e| (#peek emacs_env, make_float) |]
   [t| Env -> CDouble -> IO (RawValue 'Regular) |])

{-# INLINE makeFloat #-}
makeFloat
  :: MonadIO m
  => Env
  -> CDouble
  -> m (RawValue 'Regular)
makeFloat = makeFloatTH


$(wrapEmacsFunc "copyStringContentsTH" Unsafe
   [e| (#peek emacs_env, copy_string_contents) |]
   [t| forall p. Env -> RawValue p -> CString -> NonNullPtr CPtrdiff -> IO CBoolean |])

{-# INLINE copyStringContents #-}
-- |  Copy the content of the Lisp string VALUE to BUFFER as an utf8
-- null-terminated string.
--
-- SIZE must point to the total size of the buffer.  If BUFFER is
-- NULL or if SIZE is not big enough, write the required buffer size
-- to SIZE and return true.
--
-- Note that SIZE must include the last null byte (e.g. "abc" needs
-- a buffer of size 4).
--
-- Return true if the string was successfully copied.
copyStringContents
  :: MonadIO m
  => Env
  -> RawValue p          -- ^ Emacs value that holds a string
  -> CString             -- ^ Destination, may be NULL
  -> NonNullPtr CPtrdiff -- ^ SIZE pointer
  -> m CBoolean
copyStringContents = copyStringContentsTH


$(wrapEmacsFunc "makeStringTH" Unsafe
   [e| (#peek emacs_env, make_string) |]
   [t| Env -> CString -> CPtrdiff -> IO (RawValue 'Regular) |])

{-# INLINE makeString #-}
makeString
  :: MonadIO m
  => Env
  -> CString  -- ^ 0-terminated utf8-encoded string.
  -> CPtrdiff -- ^ Length.
  -> m (RawValue 'Regular)
makeString = makeStringTH


$(wrapEmacsFunc "makeUserPtrTH" Unsafe
   [e| (#peek emacs_env, make_user_ptr) |]
   [t| forall a. Env -> FinalizerPtr a -> Ptr a -> IO (RawValue 'Regular) |])

{-# INLINE makeUserPtr #-}
makeUserPtr
  :: forall m a. MonadIO m
  => Env
  -> FinalizerPtr a
  -> Ptr a
  -> m (RawValue 'Regular)
makeUserPtr = makeUserPtrTH


$(wrapEmacsFunc "getUserPtrTH" Unsafe
   [e| (#peek emacs_env, get_user_ptr) |]
   [t| forall p a. Env -> RawValue p -> IO (Ptr a) |])

{-# INLINE getUserPtr #-}
getUserPtr
  :: MonadIO m
  => Env
  -> RawValue p
  -> m (Ptr a)
getUserPtr = getUserPtrTH


$(wrapEmacsFunc "setUserPtrTH" Unsafe
   [e| (#peek emacs_env, set_user_ptr) |]
   [t| forall p a. Env -> RawValue p -> Ptr a -> IO () |])

{-# INLINE setUserPtr #-}
setUserPtr
  :: MonadIO m
  => Env
  -> RawValue p
  -> Ptr a
  -> m ()
setUserPtr = setUserPtrTH


$(wrapEmacsFunc "getUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, get_user_finalizer) |]
   [t| forall p a. Env -> RawValue p -> IO (FinalizerPtr a) |])

{-# INLINE getUserFinaliser #-}
getUserFinaliser
  :: MonadIO m
  => Env
  -> RawValue p
  -> m (FinalizerPtr a)
getUserFinaliser = getUserFinaliserTH


$(wrapEmacsFunc "setUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, set_user_finalizer) |]
   [t| forall p a. Env -> RawValue p -> FinalizerPtr a -> IO () |])

{-# INLINE setUserFinaliser #-}
setUserFinaliser
  :: MonadIO m
  => Env
  -> RawValue p
  -> FinalizerPtr a
  -> m ()
setUserFinaliser = setUserFinaliserTH


$(wrapEmacsFunc "vecGetTH" Unsafe
   [e| (#peek emacs_env, vec_get) |]
   [t| forall p. Env -> RawValue p -> CPtrdiff -> IO (RawValue 'Regular) |])

{-# INLINE vecGet #-}
vecGet
  :: MonadIO m
  => Env
  -> RawValue p
  -> CPtrdiff
  -> m (RawValue 'Regular)
vecGet = vecGetTH


$(wrapEmacsFunc "vecSetTH" Unsafe
   [e| (#peek emacs_env, vec_set) |]
   [t| forall p1 p2. Env -> RawValue p1 -> CPtrdiff -> RawValue p2 -> IO () |])

{-# INLINE vecSet #-}
vecSet
  :: MonadIO m
  => Env
  -> RawValue p1
  -> CPtrdiff
  -> RawValue p2
  -> m ()
vecSet = vecSetTH


$(wrapEmacsFunc "vecSizeTH" Unsafe
   [e| (#peek emacs_env, vec_size) |]
   [t| forall p. Env -> RawValue p -> IO CPtrdiff |])

{-# INLINE vecSize #-}
vecSize
  :: MonadIO m
  => Env
  -> RawValue p
  -> m CPtrdiff
vecSize = vecSizeTH


-- These are mostly of administrative interest.

$(wrapEmacsFunc "setFunctionFinalizerTH" Unsafe
   [e| (#peek emacs_env, set_function_finalizer) |]
   [t| forall p a. Env -> RawValue p -> FinalizerPtr a -> IO () |])

{-# INLINE setFunctionFinalizer #-}
setFunctionFinalizer
  :: MonadIO m
  => Env
  -> RawValue p
  -> FinalizerPtr a
  -> m ()
setFunctionFinalizer = setFunctionFinalizerTH

$(wrapEmacsFunc "processInputTH" Unsafe
   [e| (#peek emacs_env, process_input) |]
   [t| Env -> IO EnumProcessInputResult |])

{-# INLINE processInput #-}
-- | Processes pending input events and returns whether the module
-- function should quit.
processInput
  :: MonadIO m
  => Env
  -> m EnumProcessInputResult
processInput = processInputTH

