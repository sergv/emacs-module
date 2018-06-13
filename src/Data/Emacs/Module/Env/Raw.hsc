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
   [t| Env -> Emacs.Value -> IO Emacs.Value |])

{-# INLINE makeGlobalRef #-}
makeGlobalRef
  :: forall m. MonadIO m
  => Env
  -> Emacs.Value
  -> m Emacs.GlobalRef
makeGlobalRef env x =
  liftIO $
    coerce
      (makeGlobalRefTH
        :: Env
        -> Emacs.Value
        -> IO Emacs.Value)
      env
      x


$(wrapEmacsFunc "freeGlobalRefTH" Unsafe
   [e| (#peek emacs_env, free_global_ref) |]
   [t| Env -> Emacs.Value -> IO () |])

{-# INLINE freeGlobalRef #-}
freeGlobalRef
  :: forall m. MonadIO m
  => Env
  -> Emacs.GlobalRef
  -> m ()
freeGlobalRef env x =
  liftIO $
  coerce
    (freeGlobalRefTH :: Env -> Emacs.Value -> IO ())
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
   [t| Env -> NonNullPtr Emacs.Value -> NonNullPtr Emacs.Value -> IO EnumFuncallExit |])

{-# INLINE nonLocalExitGet #-}
nonLocalExitGet
  :: MonadIO m
  => Env
  -> NonNullPtr Emacs.Value -- ^ Symbol output
  -> NonNullPtr Emacs.Value -- ^ Data output
  -> m EnumFuncallExit
nonLocalExitGet = nonLocalExitGetTH


$(wrapEmacsFunc "nonLocalExitSignalTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_signal) |]
   [t| Env -> Emacs.Value -> Emacs.Value -> IO () |])

{-# INLINE nonLocalExitSignal #-}
nonLocalExitSignal
  :: MonadIO m
  => Env
  -> Emacs.Value -- ^ Error symbol
  -> Emacs.Value -- ^ Error data
  -> m ()
nonLocalExitSignal = nonLocalExitSignalTH


$(wrapEmacsFunc "nonLocalExitThrowTH" Unsafe
   [e| (#peek emacs_env, non_local_exit_throw) |]
   [t| Env -> Emacs.Value -> Emacs.Value -> IO () |])

{-# INLINE nonLocalExitThrow #-}
nonLocalExitThrow
  :: MonadIO m
  => Env
  -> Emacs.Value -- ^ Tag, a symbol
  -> Emacs.Value -- ^ Value
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
   [t| forall a. Env -> CPtrdiff -> CPtrdiff -> FunPtr (RawFunctionType a) -> CString -> Ptr a -> IO Emacs.Value |])

{-# INLINE makeFunction #-}
makeFunction
  :: forall m a. MonadIO m
  => Env
  -> CPtrdiff      -- ^ Minimum arity
  -> CPtrdiff      -- ^ Maximum arity
  -> RawFunction a -- ^ Implementation
  -> CString       -- ^ Documentation
  -> Ptr a         -- ^ Extra data
  -> m Emacs.Value
makeFunction =
  coerce
    (makeFunctionTH ::
         Env
      -> CPtrdiff
      -> CPtrdiff
      -> FunPtr (RawFunctionType a)
      -> CString
      -> Ptr a
      -> m Emacs.Value)


$(wrapEmacsFunc "funcallTH" Safe
   [e| (#peek emacs_env, funcall) |]
   [t| Env -> Emacs.Value -> CPtrdiff -> NonNullPtr Emacs.Value -> IO Emacs.Value |])

{-# INLINE funcall #-}
funcall
  :: MonadIO m
  => Env
  -> Emacs.Value            -- ^ Function
  -> CPtrdiff               -- ^ Number of arguments
  -> NonNullPtr Emacs.Value -- ^ Actual arguments
  -> m Emacs.Value
funcall = funcallTH


$(wrapEmacsFunc "funcallPrimitiveTH" Unsafe
   [e| (#peek emacs_env, funcall) |]
   [t| Env -> Emacs.Value -> CPtrdiff -> NonNullPtr Emacs.Value -> IO Emacs.Value |])

{-# INLINE funcallPrimitive #-}
funcallPrimitive
  :: MonadIO m
  => Env
  -> Emacs.Value            -- ^ Function
  -> CPtrdiff               -- ^ Number of arguments
  -> NonNullPtr Emacs.Value -- ^ Actual arguments
  -> m Emacs.Value
funcallPrimitive = funcallPrimitiveTH


$(wrapEmacsFunc "internTH" Unsafe
   [e| (#peek emacs_env, intern) |]
   [t| Env -> CString -> IO Emacs.Value |])

{-# INLINE intern #-}
intern
  :: MonadIO m
  => Env
  -> CString
  -> m Emacs.Value
intern = internTH


$(wrapEmacsFunc "typeOfTH" Unsafe
   [e| (#peek emacs_env, type_of) |]
   [t| Env -> Emacs.Value -> IO Emacs.Value |])

{-# INLINE typeOf #-}
typeOf
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> m Emacs.Value
typeOf = typeOfTH


$(wrapEmacsFunc "isNotNilTH" Unsafe
   [e| (#peek emacs_env, is_not_nil) |]
   [t| Env -> Emacs.Value -> IO CBoolean |])

{-# INLINE isNotNil #-}
isNotNil
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> m CBoolean
isNotNil = isNotNilTH


$(wrapEmacsFunc "eqTH" Unsafe
   [e| (#peek emacs_env, eq) |]
   [t| Env -> Emacs.Value -> Emacs.Value -> IO CBoolean |])

{-# INLINE eq #-}
eq
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> Emacs.Value
  -> m CBoolean
eq = eqTH


$(wrapEmacsFunc "extractIntegerTH" Unsafe
   [e| (#peek emacs_env, extract_integer) |]
   [t| Env -> Emacs.Value -> IO CIntMax |])

{-# INLINE extractInteger #-}
extractInteger
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> m CIntMax
extractInteger = extractIntegerTH


$(wrapEmacsFunc "makeIntegerTH" Unsafe
   [e| (#peek emacs_env, make_integer) |]
   [t| Env -> CIntMax -> IO Emacs.Value |])

{-# INLINE makeInteger #-}
makeInteger
  :: MonadIO m
  => Env
  -> CIntMax
  -> m Emacs.Value
makeInteger = makeIntegerTH


$(wrapEmacsFunc "extractFloatTH" Unsafe
   [e| (#peek emacs_env, extract_float) |]
   [t| Env -> Emacs.Value -> IO CDouble |])

{-# INLINE extractFloat #-}
extractFloat
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> m CDouble
extractFloat = extractFloatTH


$(wrapEmacsFunc "makeFloatTH" Unsafe
   [e| (#peek emacs_env, make_float) |]
   [t| Env -> CDouble -> IO Emacs.Value |])

{-# INLINE makeFloat #-}
makeFloat
  :: MonadIO m
  => Env
  -> CDouble
  -> m Emacs.Value
makeFloat = makeFloatTH


$(wrapEmacsFunc "copyStringContentsTH" Unsafe
   [e| (#peek emacs_env, copy_string_contents) |]
   [t| Env -> Emacs.Value -> CString -> NonNullPtr CPtrdiff -> IO CBoolean |])

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
  -> Emacs.Value         -- ^ Emacs value that holds a string
  -> CString             -- ^ Destination, may be NULL
  -> NonNullPtr CPtrdiff -- ^ SIZE pointer
  -> m CBoolean
copyStringContents = copyStringContentsTH


$(wrapEmacsFunc "makeStringTH" Unsafe
   [e| (#peek emacs_env, make_string) |]
   [t| Env -> CString -> CPtrdiff -> IO Emacs.Value |])

{-# INLINE makeString #-}
makeString
  :: MonadIO m
  => Env
  -> CString  -- ^ 0-terminated utf8-encoded string.
  -> CPtrdiff -- ^ Length.
  -> m Emacs.Value
makeString = makeStringTH


$(wrapEmacsFunc "makeUserPtrTH" Unsafe
   [e| (#peek emacs_env, make_user_ptr) |]
   [t| forall a. Env -> UserPtrFinaliser a -> Ptr a -> IO Emacs.Value |])

{-# INLINE makeUserPtr #-}
makeUserPtr
  :: forall m a. MonadIO m
  => Env
  -> UserPtrFinaliser a
  -> Ptr a
  -> m Emacs.Value
makeUserPtr = makeUserPtrTH


$(wrapEmacsFunc "getUserPtrTH" Unsafe
   [e| (#peek emacs_env, get_user_ptr) |]
   [t| forall a. Env -> Emacs.Value -> IO (Ptr a) |])

{-# INLINE getUserPtr #-}
getUserPtr
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> m (Ptr a)
getUserPtr = getUserPtrTH


$(wrapEmacsFunc "setUserPtrTH" Unsafe
   [e| (#peek emacs_env, set_user_ptr) |]
   [t| forall a. Env -> Emacs.Value -> Ptr a -> IO () |])

{-# INLINE setUserPtr #-}
setUserPtr
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> Ptr a
  -> m ()
setUserPtr = setUserPtrTH


$(wrapEmacsFunc "getUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, get_user_finalizer) |]
   [t| forall a. Env -> Emacs.Value -> IO (UserPtrFinaliser a) |])

{-# INLINE getUserFinaliser #-}
getUserFinaliser
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> m (UserPtrFinaliser a)
getUserFinaliser = getUserFinaliserTH


$(wrapEmacsFunc "setUserFinaliserTH" Unsafe
   [e| (#peek emacs_env, set_user_finalizer) |]
   [t| forall a. Env -> Emacs.Value -> UserPtrFinaliser a -> IO () |])

{-# INLINE setUserFinaliser #-}
setUserFinaliser
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> UserPtrFinaliser a
  -> m ()
setUserFinaliser = setUserFinaliserTH


$(wrapEmacsFunc "vecGetTH" Unsafe
   [e| (#peek emacs_env, vec_get) |]
   [t| Env -> Emacs.Value -> CPtrdiff -> IO Emacs.Value |])

{-# INLINE vecGet #-}
vecGet
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> CPtrdiff
  -> m Emacs.Value
vecGet = vecGetTH


$(wrapEmacsFunc "vecSetTH" Unsafe
   [e| (#peek emacs_env, vec_set) |]
   [t| Env -> Emacs.Value -> CPtrdiff -> Emacs.Value -> IO () |])

{-# INLINE vecSet #-}
vecSet
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> CPtrdiff
  -> Emacs.Value
  -> m ()
vecSet = vecSetTH


$(wrapEmacsFunc "vecSizeTH" Unsafe
   [e| (#peek emacs_env, vec_size) |]
   [t| Env -> Emacs.Value -> IO CPtrdiff |])

{-# INLINE vecSize #-}
vecSize
  :: MonadIO m
  => Env
  -> Emacs.Value
  -> m CPtrdiff
vecSize = vecSizeTH

