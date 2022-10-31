----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Monad.Async.Impl
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Emacs.Module.Monad.Async.Impl
  ( EmacsRes(..)
  , processCalls
  , callEmacs

  -- * Reexports
  , Some
  , mkSome
  ) where

import Prelude hiding (min, max)

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.ByteString.Unsafe qualified as BSU
import Data.Some.Newtype
import Foreign.C.Types
import Foreign.Ptr

import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env qualified as Env
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.SymbolName.Internal
import Emacs.Module.Assert
import Emacs.Module.EmacsCall
import Emacs.Module.Monad.Common as Common

processCalls
  :: WithCallStack
  => Env
  -> TMQueue (Some (EmacsCall EmacsRes MVar))
  -> IO ()
processCalls env reqs =
  withNonLocalState $ \nls ->
    let go =
          atomically (readTMQueue reqs) >>= \case
            Just x  -> withSome x (callEmacs env nls) *> go
            Nothing -> pure ()
    in go

callEmacs
  :: WithCallStack
  => Env
  -> NonLocalState
  -> EmacsCall EmacsRes MVar a
  -> IO ()
callEmacs env nls = \case
  MakeGlobalRef x out   -> Env.makeGlobalRef env x >>= putMVar out
  FreeGlobalRef x       -> Env.freeGlobalRef env x

  NonLocalExitCheck out ->
    Env.nonLocalExitCheck env >>= Common.unpackEnumFuncallExit >>= putMVar out
  NonLocalExitGet out   ->
    putMVar out =<< Common.nonLocalExitGet env nls
  NonLocalExitSignal callStack sym dat out ->
    putMVar out =<< Common.nonLocalExitSignal env callStack sym dat

  NonLocalExitThrow tag val -> Env.nonLocalExitThrow env tag val
  NonLocalExitClear         -> Env.nonLocalExitClear env

  MakeFunction min max impl doc out ->
    Doc.useDocAsCString doc $ \doc' -> do
      func <- Env.makeFunction env (fromIntegral min) (fromIntegral max) impl doc' (castFunPtrToPtr (unRawFunction impl))
      Env.setFunctionFinalizer env func freeHaskellFunPtrWrapped
      putMVar out func

  Funcall func args out -> do
    withPtrLenNonNull args $ \n args' ->
      putMVar out
        =<< checkNonLocalExitFull env nls
        =<< Env.funcall env func (fromIntegral n) args'

  FuncallPrimitive func args out ->
    withPtrLenNonNull args $ \n args' ->
      putMVar out
        =<< checkNonLocalExitFull env nls
        =<< Env.funcallPrimitive env func (fromIntegral n) args'

  FuncallPrimitiveUnchecked func args out ->
    withPtrLenNonNull args $ \n args' ->
      putMVar out =<< Env.funcallPrimitive env func (fromIntegral n) args'

  Intern sym out ->
    putMVar out =<< reifySymbolUnknown env sym

  TypeOf x out ->
    putMVar out =<< Env.typeOf env x

  IsNotNil x out ->
    putMVar out . Env.isTruthy =<< Env.isNotNil env x

  Eq x y out ->
    putMVar out . Env.isTruthy =<< Env.eq env x y

  ExtractInteger x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "ExtractInteger" . fromIntegral
      =<< Env.extractInteger env x

  MakeInteger x out ->
    putMVar out =<< Env.makeInteger env (fromIntegral x)

  ExtractFloat x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "ExtractFloat" . (\(CDouble y) -> y)
      =<< Env.extractFloat env x

  MakeFloat x out ->
    putMVar out =<< Env.makeFloat env (CDouble x)

  ExtractString x out ->
    putMVar out =<< Common.extractString env nls x

  MakeString x out ->
    BSU.unsafeUseAsCStringLen x $ \(pStr, len) ->
      putMVar out =<< Env.makeString env pStr (fromIntegral len)

  GetUserPtr x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "GetUserPtr"
      =<< Env.getUserPtr env x

  MakeUserPtr fin ptr out ->
    putMVar out =<< Env.makeUserPtr env fin ptr

  SetUserPtr dest ptr out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "SetUserPtr"
      =<< Env.setUserPtr env dest ptr

  GetUserPtrFinaliser x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "GetUserPtrFinaliser"
      =<< Env.getUserFinaliser env x

  SetUserPtrFinaliser x fin out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "SetUserPtrFinaliser"
      =<< Env.setUserFinaliser env x fin

  VecGet vec n out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "VecGet"
      =<< Env.vecGet env vec (fromIntegral n)

  VecSet vec n x out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "VecSet"
      =<< Env.vecSet env vec (fromIntegral n) x

  VecSize vec out ->
    putMVar out
      =<< checkNonLocalExitSignal env nls "VecSize" . fromIntegral
      =<< Env.vecSize env vec

  ProcessInput -> processInput env
