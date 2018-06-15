----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.Raw
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

module Data.Emacs.Module.Env.Raw
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

import Data.Emacs.Module.Env.Internal as Env
import Data.Emacs.Module.Env.Raw.TH
import Data.Emacs.Module.NonNullPtr
import qualified Data.Emacs.Module.Value as Emacs

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
   [t| Env -> Emacs.RawValue -> IO Emacs.RawValue |])

{-# INLINE makeGlobalRef #-}
makeGlobalRef
  :: forall m. MonadIO m
  => Env
  -> Emacs.RawValue
  -> m Emacs.GlobalRef
makeGlobalRef env x =
  liftIO $
    coerce
      (makeGlobalRefTH
        :: Env
        -> Emacs.RawValue
        -> IO Emacs.RawValue)
      env
      x


$(wrapEmacsFunc "freeGlobalRefTH" Unsafe
   [e| (#peek emacs_env, free_global_ref) |]
   [t| Env -> Emacs.RawValue -> IO () |])

{-# INLINE freeGlobalRef #-}
freeGlobalRef
  :: forall m. MonadIO m
  => Env
  -> Emacs.GlobalRef
  -> m ()
freeGlobalRef env x =
  liftIO $
  coerce
    (freeGlobalRefTH :: Env -> Emacs.RawValue -> IO ())
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
   [t| Env -> NonNullPtr Emacs.RawValue -> NonNullPtr Emacs.RawValue -> IO EnumFuncallExit |])

{-# INLINE nonLocalExitGet #-}
nonLocalExitGet
  :: MonadIO m
  => Env
  -> NonNullPtr Emacs.RawValue -- ^ Symbol output
  -> NonNullPtr Emacs.RawValue -- ^ Data output
  -> m EnumFuncallExit
nonLocalExitGet = nonLocalExitGetTH


$(wrapEmacsFunc "nonLocalExitSignalTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_signal) |]
   [t| Env -> Emacs.RawValue -> Emacs.RawValue -> IO () |])

{-# INLINE nonLocalExitSignal #-}
nonLocalExitSignal
  :: MonadIO m
  => Env
  -> Emacs.RawValue -- ^ Error symbol
  -> Emacs.RawValue -- ^ Error data
  -> m ()
nonLocalExitSignal = nonLocalExitSignalTH


$(wrapEmacsFunc "nonLocalExitThrowTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_throw) |]
   [t| Env -> Emacs.RawValue -> Emacs.RawValue -> IO () |])

{-# INLINE nonLocalExitThrow #-}
nonLocalExitThrow
  :: MonadIO m
  => Env
  -> Emacs.RawValue -- ^ Tag, a symbol
  -> Emacs.RawValue -- ^ Value
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
   [t| forall a. Env -> CPtrdiff -> CPtrdiff -> FunPtr (RawFunctionType a) -> CString -> Ptr a -> IO Emacs.RawValue |])

{-# INLINE makeFunction #-}
makeFunction
  :: forall m a. MonadIO m
  => Env
  -> CPtrdiff      -- ^ Minimum arity
  -> CPtrdiff      -- ^ Maximum arity
  -> RawFunction a -- ^ Implementation
  -> CString       -- ^ Documentation
  -> Ptr a         -- ^ Extra data
  -> m Emacs.RawValue
makeFunction =
  coerce
    (makeFunctionTH ::
         Env
      -> CPtrdiff
      -> CPtrdiff
      -> FunPtr (RawFunctionType a)
      -> CString
      -> Ptr a
      -> m Emacs.RawValue)


$(wrapEmacsFunc "funcallTH" Safe
   [e| (#peek emacs_env, funcall) |]
   [t| Env -> Emacs.RawValue -> CPtrdiff -> NonNullPtr Emacs.RawValue -> IO Emacs.RawValue |])

{-# INLINE funcall #-}
funcall
  :: MonadIO m
  => Env
  -> Emacs.RawValue            -- ^ Function
  -> CPtrdiff                  -- ^ Number of arguments
  -> NonNullPtr Emacs.RawValue -- ^ Actual arguments
  -> m Emacs.RawValue
funcall = funcallTH


$(wrapEmacsFunc "funcallPrimitiveTH" Unsafe
   [e| (#peek emacs_env, funcall) |]
   [t| Env -> Emacs.RawValue -> CPtrdiff -> NonNullPtr Emacs.RawValue -> IO Emacs.RawValue |])

{-# INLINE funcallPrimitive #-}
funcallPrimitive
  :: MonadIO m
  => Env
  -> Emacs.RawValue            -- ^ Function
  -> CPtrdiff                  -- ^ Number of arguments
  -> NonNullPtr Emacs.RawValue -- ^ Actual arguments
  -> m Emacs.RawValue
funcallPrimitive = funcallPrimitiveTH


$(wrapEmacsFunc "internTH" Unsafe
   [e| (#peek emacs_env, intern) |]
   [t| Env -> CString -> IO Emacs.RawValue |])

{-# INLINE intern #-}
intern
  :: MonadIO m
  => Env
  -> CString
  -> m Emacs.RawValue
intern = internTH


$(wrapEmacsFunc "typeOfTH" Unsafe
   [e| (#peek emacs_env, type_of) |]
   [t| Env -> Emacs.RawValue -> IO Emacs.RawValue |])

{-# INLINE typeOf #-}
typeOf
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> m Emacs.RawValue
typeOf = typeOfTH


$(wrapEmacsFunc "isNotNilTH" Unsafe
   [e| (#peek emacs_env, is_not_nil) |]
   [t| Env -> Emacs.RawValue -> IO CBoolean |])

{-# INLINE isNotNil #-}
isNotNil
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> m CBoolean
isNotNil = isNotNilTH


$(wrapEmacsFunc "eqTH" Unsafe
   [e| (#peek emacs_env, eq) |]
   [t| Env -> Emacs.RawValue -> Emacs.RawValue -> IO CBoolean |])

{-# INLINE eq #-}
eq
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> Emacs.RawValue
  -> m CBoolean
eq = eqTH


$(wrapEmacsFunc "extractIntegerTH" Unsafe
   [e| (#peek emacs_env, extract_integer) |]
   [t| Env -> Emacs.RawValue -> IO CIntMax |])

{-# INLINE extractInteger #-}
extractInteger
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> m CIntMax
extractInteger = extractIntegerTH


$(wrapEmacsFunc "makeIntegerTH" Unsafe
   [e| (#peek emacs_env, make_integer) |]
   [t| Env -> CIntMax -> IO Emacs.RawValue |])

{-# INLINE makeInteger #-}
makeInteger
  :: MonadIO m
  => Env
  -> CIntMax
  -> m Emacs.RawValue
makeInteger = makeIntegerTH


$(wrapEmacsFunc "extractFloatTH" Unsafe
   [e| (#peek emacs_env, extract_float) |]
   [t| Env -> Emacs.RawValue -> IO CDouble |])

{-# INLINE extractFloat #-}
extractFloat
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> m CDouble
extractFloat = extractFloatTH


$(wrapEmacsFunc "makeFloatTH" Unsafe
   [e| (#peek emacs_env, make_float) |]
   [t| Env -> CDouble -> IO Emacs.RawValue |])

{-# INLINE makeFloat #-}
makeFloat
  :: MonadIO m
  => Env
  -> CDouble
  -> m Emacs.RawValue
makeFloat = makeFloatTH


$(wrapEmacsFunc "copyStringContentsTH" Unsafe
   [e| (#peek emacs_env, copy_string_contents) |]
   [t| Env -> Emacs.RawValue -> CString -> NonNullPtr CPtrdiff -> IO CBoolean |])

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
  -> Emacs.RawValue      -- ^ Emacs value that holds a string
  -> CString             -- ^ Destination, may be NULL
  -> NonNullPtr CPtrdiff -- ^ SIZE pointer
  -> m CBoolean
copyStringContents = copyStringContentsTH


$(wrapEmacsFunc "makeStringTH" Unsafe
   [e| (#peek emacs_env, make_string) |]
   [t| Env -> CString -> CPtrdiff -> IO Emacs.RawValue |])

{-# INLINE makeString #-}
makeString
  :: MonadIO m
  => Env
  -> CString  -- ^ 0-terminated utf8-encoded string.
  -> CPtrdiff -- ^ Length.
  -> m Emacs.RawValue
makeString = makeStringTH


$(wrapEmacsFunc "makeUserPtrTH" Unsafe
   [e| (#peek emacs_env, make_user_ptr) |]
   [t| forall a. Env -> UserPtrFinaliser a -> Ptr a -> IO Emacs.RawValue |])

{-# INLINE makeUserPtr #-}
makeUserPtr
  :: forall m a. MonadIO m
  => Env
  -> UserPtrFinaliser a
  -> Ptr a
  -> m Emacs.RawValue
makeUserPtr = makeUserPtrTH


$(wrapEmacsFunc "getUserPtrTH" Unsafe
   [e| (#peek emacs_env, get_user_ptr) |]
   [t| forall a. Env -> Emacs.RawValue -> IO (Ptr a) |])

{-# INLINE getUserPtr #-}
getUserPtr
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> m (Ptr a)
getUserPtr = getUserPtrTH


$(wrapEmacsFunc "setUserPtrTH" Unsafe
   [e| (#peek emacs_env, set_user_ptr) |]
   [t| forall a. Env -> Emacs.RawValue -> Ptr a -> IO () |])

{-# INLINE setUserPtr #-}
setUserPtr
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> Ptr a
  -> m ()
setUserPtr = setUserPtrTH


$(wrapEmacsFunc "getUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, get_user_finalizer) |]
   [t| forall a. Env -> Emacs.RawValue -> IO (UserPtrFinaliser a) |])

{-# INLINE getUserFinaliser #-}
getUserFinaliser
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> m (UserPtrFinaliser a)
getUserFinaliser = getUserFinaliserTH


$(wrapEmacsFunc "setUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, set_user_finalizer) |]
   [t| forall a. Env -> Emacs.RawValue -> UserPtrFinaliser a -> IO () |])

{-# INLINE setUserFinaliser #-}
setUserFinaliser
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> UserPtrFinaliser a
  -> m ()
setUserFinaliser = setUserFinaliserTH


$(wrapEmacsFunc "vecGetTH" Unsafe
   [e| (#peek emacs_env, vec_get) |]
   [t| Env -> Emacs.RawValue -> CPtrdiff -> IO Emacs.RawValue |])

{-# INLINE vecGet #-}
vecGet
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> CPtrdiff
  -> m Emacs.RawValue
vecGet = vecGetTH


$(wrapEmacsFunc "vecSetTH" Unsafe
   [e| (#peek emacs_env, vec_set) |]
   [t| Env -> Emacs.RawValue -> CPtrdiff -> Emacs.RawValue -> IO () |])

{-# INLINE vecSet #-}
vecSet
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> CPtrdiff
  -> Emacs.RawValue
  -> m ()
vecSet = vecSetTH


$(wrapEmacsFunc "vecSizeTH" Unsafe
   [e| (#peek emacs_env, vec_size) |]
   [t| Env -> Emacs.RawValue -> IO CPtrdiff |])

{-# INLINE vecSize #-}
vecSize
  :: MonadIO m
  => Env
  -> Emacs.RawValue
  -> m CPtrdiff
vecSize = vecSizeTH

