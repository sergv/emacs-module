----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Env
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Low-level and, hopefully, low-overhead wrappers around @struct emacs_env@.
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.Emacs.Module.Raw.Env
  ( EnumFuncallExit(..)
  , UserPtrFinaliserType
  , UserPtrFinaliser
  , CBoolean
  , isTruthy
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
  ) where

import Control.Monad.IO.Class

import Data.Coerce
import Foreign
import Foreign.C

import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env.Internal as Env
import Data.Emacs.Module.Raw.Env.TH
import Data.Emacs.Module.Raw.Value

import Data.Emacs.Module.NonNullPtr.Internal

#include <emacs-module.h>

newtype EnumFuncallExit = EnumFuncallExit { unEnumFuncallExit :: CInt }

type UserPtrFinaliserType a = Ptr a -> IO ()
type UserPtrFinaliser a = FunPtr (UserPtrFinaliserType a)

-- | A wrapper around C value that denotes true or false.
newtype CBoolean = CBoolean (#type bool)

{-# INLINE isTruthy #-}
-- | Check whether a 'CBoolean' denotes true.
isTruthy :: CBoolean -> Bool
isTruthy (CBoolean a) = a /= 0


{-# INLINE isValidEnv #-}
-- | Check wheter passed @emacs_env@ structure has expected size so that
-- we will be able to access all of its fields.
isValidEnv :: MonadIO m => Env -> m Bool
isValidEnv env = liftIO $ do
  realSize <- (#peek emacs_env, size) (Env.toPtr env)
  pure $ expectedSize <= realSize
  where
    expectedSize :: CPtrdiff
    expectedSize = (#size emacs_env)

$(wrapEmacsFunc "makeGlobalRefTH" Unsafe
   [e| (#peek emacs_env, make_global_ref) |]
   [t| Env -> RawValue -> IO RawValue |])

{-# INLINE makeGlobalRef #-}
makeGlobalRef
  :: forall m. MonadIO m
  => Env
  -> RawValue
  -> m GlobalRef
makeGlobalRef env x =
  liftIO $
    coerce
      (makeGlobalRefTH
        :: Env
        -> RawValue
        -> IO RawValue)
      env
      x


$(wrapEmacsFunc "freeGlobalRefTH" Unsafe
   [e| (#peek emacs_env, free_global_ref) |]
   [t| Env -> RawValue -> IO () |])

{-# INLINE freeGlobalRef #-}
freeGlobalRef
  :: forall m. MonadIO m
  => Env
  -> GlobalRef
  -> m ()
freeGlobalRef env x =
  liftIO $
  coerce
    (freeGlobalRefTH :: Env -> RawValue -> IO ())
    env
    x


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
   [t| Env -> NonNullPtr RawValue -> NonNullPtr RawValue -> IO EnumFuncallExit |])

{-# INLINE nonLocalExitGet #-}
nonLocalExitGet
  :: MonadIO m
  => Env
  -> NonNullPtr RawValue -- ^ Symbol output
  -> NonNullPtr RawValue -- ^ Data output
  -> m EnumFuncallExit
nonLocalExitGet = nonLocalExitGetTH


$(wrapEmacsFunc "nonLocalExitSignalTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_signal) |]
   [t| Env -> RawValue -> RawValue -> IO () |])

{-# INLINE nonLocalExitSignal #-}
nonLocalExitSignal
  :: MonadIO m
  => Env
  -> RawValue -- ^ Error symbol
  -> RawValue -- ^ Error data
  -> m ()
nonLocalExitSignal = nonLocalExitSignalTH


$(wrapEmacsFunc "nonLocalExitThrowTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_throw) |]
   [t| Env -> RawValue -> RawValue -> IO () |])

{-# INLINE nonLocalExitThrow #-}
nonLocalExitThrow
  :: MonadIO m
  => Env
  -> RawValue -- ^ Tag, a symbol
  -> RawValue -- ^ Value
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
   [t| forall a. Env -> CPtrdiff -> CPtrdiff -> FunPtr (RawFunctionType a) -> CString -> Ptr a -> IO RawValue |])

{-# INLINE makeFunction #-}
makeFunction
  :: forall m a. MonadIO m
  => Env
  -> CPtrdiff      -- ^ Minimum arity
  -> CPtrdiff      -- ^ Maximum arity
  -> RawFunction a -- ^ Implementation
  -> CString       -- ^ Documentation
  -> Ptr a         -- ^ Extra data
  -> m RawValue
makeFunction =
  coerce
    (makeFunctionTH ::
         Env
      -> CPtrdiff
      -> CPtrdiff
      -> FunPtr (RawFunctionType a)
      -> CString
      -> Ptr a
      -> m RawValue)


$(wrapEmacsFunc "funcallTH" Safe
   [e| (#peek emacs_env, funcall) |]
   [t| Env -> RawValue -> CPtrdiff -> NonNullPtr RawValue -> IO RawValue |])

{-# INLINE funcall #-}
funcall
  :: MonadIO m
  => Env
  -> RawValue            -- ^ Function
  -> CPtrdiff            -- ^ Number of arguments
  -> NonNullPtr RawValue -- ^ Actual arguments
  -> m RawValue
funcall = funcallTH


$(wrapEmacsFunc "funcallPrimitiveTH" Unsafe
   [e| (#peek emacs_env, funcall) |]
   [t| Env -> RawValue -> CPtrdiff -> NonNullPtr RawValue -> IO RawValue |])

{-# INLINE funcallPrimitive #-}
funcallPrimitive
  :: MonadIO m
  => Env
  -> RawValue            -- ^ Function
  -> CPtrdiff            -- ^ Number of arguments
  -> NonNullPtr RawValue -- ^ Actual arguments
  -> m RawValue
funcallPrimitive = funcallPrimitiveTH


$(wrapEmacsFunc "internTH" Unsafe
   [e| (#peek emacs_env, intern) |]
   [t| Env -> CString -> IO RawValue |])

{-# INLINE intern #-}
intern
  :: MonadIO m
  => Env
  -> CString
  -> m RawValue
intern = internTH


$(wrapEmacsFunc "typeOfTH" Unsafe
   [e| (#peek emacs_env, type_of) |]
   [t| Env -> RawValue -> IO RawValue |])

{-# INLINE typeOf #-}
typeOf
  :: MonadIO m
  => Env
  -> RawValue
  -> m RawValue
typeOf = typeOfTH


$(wrapEmacsFunc "isNotNilTH" Unsafe
   [e| (#peek emacs_env, is_not_nil) |]
   [t| Env -> RawValue -> IO CBoolean |])

{-# INLINE isNotNil #-}
isNotNil
  :: MonadIO m
  => Env
  -> RawValue
  -> m CBoolean
isNotNil = isNotNilTH


$(wrapEmacsFunc "eqTH" Unsafe
   [e| (#peek emacs_env, eq) |]
   [t| Env -> RawValue -> RawValue -> IO CBoolean |])

{-# INLINE eq #-}
eq
  :: MonadIO m
  => Env
  -> RawValue
  -> RawValue
  -> m CBoolean
eq = eqTH


$(wrapEmacsFunc "extractIntegerTH" Unsafe
   [e| (#peek emacs_env, extract_integer) |]
   [t| Env -> RawValue -> IO CIntMax |])

{-# INLINE extractInteger #-}
extractInteger
  :: MonadIO m
  => Env
  -> RawValue
  -> m CIntMax
extractInteger = extractIntegerTH


$(wrapEmacsFunc "makeIntegerTH" Unsafe
   [e| (#peek emacs_env, make_integer) |]
   [t| Env -> CIntMax -> IO RawValue |])

{-# INLINE makeInteger #-}
makeInteger
  :: MonadIO m
  => Env
  -> CIntMax
  -> m RawValue
makeInteger = makeIntegerTH


$(wrapEmacsFunc "extractFloatTH" Unsafe
   [e| (#peek emacs_env, extract_float) |]
   [t| Env -> RawValue -> IO CDouble |])

{-# INLINE extractFloat #-}
extractFloat
  :: MonadIO m
  => Env
  -> RawValue
  -> m CDouble
extractFloat = extractFloatTH


$(wrapEmacsFunc "makeFloatTH" Unsafe
   [e| (#peek emacs_env, make_float) |]
   [t| Env -> CDouble -> IO RawValue |])

{-# INLINE makeFloat #-}
makeFloat
  :: MonadIO m
  => Env
  -> CDouble
  -> m RawValue
makeFloat = makeFloatTH


$(wrapEmacsFunc "copyStringContentsTH" Unsafe
   [e| (#peek emacs_env, copy_string_contents) |]
   [t| Env -> RawValue -> CString -> NonNullPtr CPtrdiff -> IO CBoolean |])

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
  -> RawValue         -- ^ Emacs value that holds a string
  -> CString             -- ^ Destination, may be NULL
  -> NonNullPtr CPtrdiff -- ^ SIZE pointer
  -> m CBoolean
copyStringContents = copyStringContentsTH


$(wrapEmacsFunc "makeStringTH" Unsafe
   [e| (#peek emacs_env, make_string) |]
   [t| Env -> CString -> CPtrdiff -> IO RawValue |])

{-# INLINE makeString #-}
makeString
  :: MonadIO m
  => Env
  -> CString  -- ^ 0-terminated utf8-encoded string.
  -> CPtrdiff -- ^ Length.
  -> m RawValue
makeString = makeStringTH


$(wrapEmacsFunc "makeUserPtrTH" Unsafe
   [e| (#peek emacs_env, make_user_ptr) |]
   [t| forall a. Env -> UserPtrFinaliser a -> Ptr a -> IO RawValue |])

{-# INLINE makeUserPtr #-}
makeUserPtr
  :: forall m a. MonadIO m
  => Env
  -> UserPtrFinaliser a
  -> Ptr a
  -> m RawValue
makeUserPtr = makeUserPtrTH


$(wrapEmacsFunc "getUserPtrTH" Unsafe
   [e| (#peek emacs_env, get_user_ptr) |]
   [t| forall a. Env -> RawValue -> IO (Ptr a) |])

{-# INLINE getUserPtr #-}
getUserPtr
  :: MonadIO m
  => Env
  -> RawValue
  -> m (Ptr a)
getUserPtr = getUserPtrTH


$(wrapEmacsFunc "setUserPtrTH" Unsafe
   [e| (#peek emacs_env, set_user_ptr) |]
   [t| forall a. Env -> RawValue -> Ptr a -> IO () |])

{-# INLINE setUserPtr #-}
setUserPtr
  :: MonadIO m
  => Env
  -> RawValue
  -> Ptr a
  -> m ()
setUserPtr = setUserPtrTH


$(wrapEmacsFunc "getUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, get_user_finalizer) |]
   [t| forall a. Env -> RawValue -> IO (UserPtrFinaliser a) |])

{-# INLINE getUserFinaliser #-}
getUserFinaliser
  :: MonadIO m
  => Env
  -> RawValue
  -> m (UserPtrFinaliser a)
getUserFinaliser = getUserFinaliserTH


$(wrapEmacsFunc "setUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, set_user_finalizer) |]
   [t| forall a. Env -> RawValue -> UserPtrFinaliser a -> IO () |])

{-# INLINE setUserFinaliser #-}
setUserFinaliser
  :: MonadIO m
  => Env
  -> RawValue
  -> UserPtrFinaliser a
  -> m ()
setUserFinaliser = setUserFinaliserTH


$(wrapEmacsFunc "vecGetTH" Unsafe
   [e| (#peek emacs_env, vec_get) |]
   [t| Env -> RawValue -> CPtrdiff -> IO RawValue |])

{-# INLINE vecGet #-}
vecGet
  :: MonadIO m
  => Env
  -> RawValue
  -> CPtrdiff
  -> m RawValue
vecGet = vecGetTH


$(wrapEmacsFunc "vecSetTH" Unsafe
   [e| (#peek emacs_env, vec_set) |]
   [t| Env -> RawValue -> CPtrdiff -> RawValue -> IO () |])

{-# INLINE vecSet #-}
vecSet
  :: MonadIO m
  => Env
  -> RawValue
  -> CPtrdiff
  -> RawValue
  -> m ()
vecSet = vecSetTH


$(wrapEmacsFunc "vecSizeTH" Unsafe
   [e| (#peek emacs_env, vec_size) |]
   [t| Env -> RawValue -> IO CPtrdiff |])

{-# INLINE vecSize #-}
vecSize
  :: MonadIO m
  => Env
  -> RawValue
  -> m CPtrdiff
vecSize = vecSizeTH

