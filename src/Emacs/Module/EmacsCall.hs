----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Env.EmacsCall
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Emacs.Module.EmacsCall
  ( EmacsCall(..)
  , EmacsRes(..)
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
import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int
import Data.Kind
import Data.Some.Newtype
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Traversable
import Data.Tuple.Homogenous
import Data.Void
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr
import GHC.Stack (callStack)
import Prettyprinter

import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.Env.Functions
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env (EnumFuncallExit(..))
import Data.Emacs.Module.Raw.Env qualified as Env
import Data.Emacs.Module.Raw.Env.Internal
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Emacs.Module.Assert
import Emacs.Module.Errors
import Foreign.Ptr.Builder as PtrBuilder

-- NB: all RawValues are stored without bangs so that they may lazily
-- be loaded by emacs. Otherwise client thread will block until the
-- actual Emacs value pointer becomes available and we don’t want that
-- since client thread may have other productive work to do in the
-- meantime.

-- | @f@ is a "failure" wrapper for results that may include error info.
data EmacsCall (f :: Type -> Type -> Type -> Type) (out :: Type -> Type) (res :: Type) where
  MakeGlobalRef
    :: RawValue 'Regular
    -> out (RawValue 'Pinned)
    -> EmacsCall f out (RawValue 'Pinned)
  FreeGlobalRef
    :: RawValue 'Pinned
    -> EmacsCall f out ()

  NonLocalExitCheck
    :: out (FuncallExit ())
    -> EmacsCall f out (FuncallExit ())
  NonLocalExitGet
    :: out (FuncallExit (RawValue 'Regular, RawValue 'Regular))
    -> EmacsCall f out (FuncallExit (RawValue 'Regular, RawValue 'Regular))
  NonLocalExitSignal
    :: RawValue 'Unknown           -- ^ Error symbol
    -> Builder (RawValue 'Regular) -- ^ Error data
    -> out EmacsSignal
    -> EmacsCall f out EmacsSignal
  NonLocalExitThrow
    :: RawValue p1 -- ^ Tag, a symbol
    -> RawValue p2 -- ^ Value
    -> EmacsCall f out a
  NonLocalExitClear
    :: EmacsCall f out ()

  MakeFunction
    :: !Int                      -- ^ Minimum arity
    -> !Int                      -- ^ Maximum arity
    -> !(RawFunction 'Unknown a) -- ^ Implementation
    -> !Doc.Doc                  -- ^ Documentation
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)
  Funcall
    -- -> Int#                -- ^ Number of arguments
    -- -> ByteArray#          -- ^ Arguments, must be pinned
    :: RawValue 'Regular           -- ^ Function
    -> Builder (RawValue 'Regular) -- ^ Arguments, lazy to not make client
                                   -- sending request block until argument
                                   -- values become available
    -> out (f EmacsSignal EmacsThrow (RawValue 'Regular))
    -> EmacsCall f out (f EmacsSignal EmacsThrow (RawValue 'Regular))
  FuncallPrimitive
    -- -> Int#                -- ^ Number of arguments
    -- -> ByteArray#          -- ^ Arguments, must be pinned
    :: RawValue 'Regular           -- ^ Function
    -> Builder (RawValue 'Regular) -- ^ Arguments, lazy to not make client
                                   -- sending request block until argument
                                   -- values become available
    -> out (f EmacsSignal EmacsThrow (RawValue 'Regular))
    -> EmacsCall f out (f EmacsSignal EmacsThrow (RawValue 'Regular))
  -- | For functions that cannot fail
  FuncallPrimitiveUnchecked
    :: RawValue 'Regular           -- ^ Function
    -> Builder (RawValue 'Regular) -- ^ Arguments, lazy to not make client
                                   -- sending request block until argument
                                   -- values become available
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  Intern
    :: SymbolName
    -> out (RawValue 'Unknown)
    -> EmacsCall f out (RawValue 'Unknown)

  TypeOf
    :: RawValue p
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  IsNotNil
    :: RawValue p
    -> out Bool
    -> EmacsCall f out Bool

  Eq
    :: RawValue p1
    -> RawValue p2
    -> out Bool
    -> EmacsCall f out Bool

  ExtractInteger
    :: RawValue p
    -> out (f EmacsSignal Void Int64)
    -> EmacsCall f out (f EmacsSignal Void Int64)
  MakeInteger
    :: !Int64
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  ExtractFloat
    :: RawValue p
    -> out (f EmacsSignal Void Double)
    -> EmacsCall f out (f EmacsSignal Void Double)
  MakeFloat
    :: !Double
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  ExtractString
    :: RawValue p
    -> out (f EmacsSignal Void BS.ByteString)
    -> EmacsCall f out (f EmacsSignal Void BS.ByteString)
  MakeString
    :: !BS.ByteString
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)

  GetUserPtr
    :: RawValue p
    -> out (f EmacsSignal Void (Ptr a))
    -> EmacsCall f out (f EmacsSignal Void (Ptr a))
  MakeUserPtr
    :: !(FinalizerPtr a)
    -> !(Ptr a)
    -> out (RawValue 'Regular)
    -> EmacsCall f out (RawValue 'Regular)
  SetUserPtr
    :: RawValue 'Regular
    -> !(Ptr a)
    -> out (f EmacsSignal Void ())
    -> EmacsCall f out (f EmacsSignal Void ())

  GetUserPtrFinaliser
    :: RawValue p
    -> out (f EmacsSignal Void (FinalizerPtr a))
    -> EmacsCall f out (f EmacsSignal Void (FinalizerPtr a))
  SetUserPtrFinaliser
    :: RawValue p
    -> !(FinalizerPtr a)
    -> out (f EmacsSignal Void ())
    -> EmacsCall f out (f EmacsSignal Void ())

  VecGet
    :: RawValue p
    -> !Int
    -> out (f EmacsSignal Void (RawValue 'Regular))
    -> EmacsCall f out (f EmacsSignal Void (RawValue 'Regular))
  VecSet
    :: RawValue p1
    -> !Int
    -> RawValue p2
    -> out (f EmacsSignal Void ())
    -> EmacsCall f out (f EmacsSignal Void ())
  VecSize
    :: RawValue p
    -> out (f EmacsSignal Void Int)
    -> EmacsCall f out (f EmacsSignal Void Int)

deriving instance (forall x. Show (out x)) => Show (EmacsCall f out a)

data EmacsRes s t a
  = EmacsSuccess a
  | EmacsExitSignal s
  | EmacsExitThrow t
  deriving (Functor, Foldable, Traversable)

unpackEnumFuncallExit
  :: WithCallStack
  => EnumFuncallExit -> IO (FuncallExit ())
unpackEnumFuncallExit =
  either throwIO pure . unpackEnumFuncallExitSafe

unpackEnumFuncallExitSafe
  :: WithCallStack
  => EnumFuncallExit -> Either EmacsInternalError (FuncallExit ())
unpackEnumFuncallExitSafe (EnumFuncallExit (CInt x)) =
  case funcallExitFromNum x of
    Nothing -> Left $ mkEmacsInternalError $
      "Unknown value of enum emacs_funcall_exit:" <+> pretty x
    Just y -> Right y

instance Show (MVar a) where
  show _ = "MVar"

processCalls
  :: WithCallStack
  => Env
  -> TMQueue (Some (EmacsCall EmacsRes MVar))
  -> IO ()
processCalls env reqs =
  allocaNonNull $ \nlsErr ->
    allocaNonNull $ \nlsData ->
      let !nls = NonLocalState{nlsErr, nlsData}
          go =
            atomically (readTMQueue reqs) >>= \case
              Just x  -> withSome x (callEmacs env nls) *> go
              Nothing -> pure ()
      in go

nonLocalExitGet
  :: WithCallStack
  => Env
  -> NonLocalState
  -> IO (FuncallExit (RawValue 'Regular, RawValue 'Regular))
nonLocalExitGet env NonLocalState{nlsErr, nlsData} = do
  x <- unpackEnumFuncallExit =<< Env.nonLocalExitGet env nlsErr nlsData
  for x $ \(_ :: ()) ->
    (,) <$> peek (unNonNullPtr nlsErr) <*> peek (unNonNullPtr nlsData)

extractString
  :: WithCallStack
  => Env
  -> NonLocalState
  -> RawValue p
  -> IO (EmacsRes EmacsSignal Void BS.ByteString)
extractString env nls x = do
  allocaNonNull $ \pSize -> do
    res <- Env.copyStringContents env x nullPtr pSize
    if Env.isNonTruthy res
    then do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError
        "Failed to obtain size when unpacking string. Probable cause: emacs object is not a string."
    else do
      size          <- fromIntegral <$> peek (unNonNullPtr pSize)
      fp            <- BSI.mallocByteString size
      copyPerformed <- unsafeWithForeignPtr fp $ \ptr ->
        Env.copyStringContents env x (castPtr ptr) pSize
      if Env.isTruthy copyPerformed
      then
        -- Should subtract 1 from size to avoid NULL terminator at the end.
        pure $ EmacsSuccess $ BSI.BS fp (size - 1)
      else do
       nonLocalExitGet env nls >>= \ case
         FuncallExitSignal (sym, dat) -> do
           -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
           Env.nonLocalExitClear env
           emacsSignalInfo <- extractSignalInfo env sym dat
           pure $ EmacsExitSignal $ EmacsSignal
             { emacsSignalSym    = toUnknown sym
             , emacsSignalData   = dat
             , emacsSignalOrigin = callStack
             , emacsSignalInfo
             }
         FuncallExitReturn            ->
           throwIO $ mkEmacsInternalError "Failed to unpack string"
         FuncallExitThrow{} ->
           throwIO $ mkEmacsInternalError
             "The copy string contents operation should have never exited via throw"

checkNonLocalExitSignal
  :: WithCallStack
  => Env
  -> NonLocalState
  -> Text
  -> a
  -> IO (EmacsRes EmacsSignal Void a)
checkNonLocalExitSignal env nls errMsg res = do
  nonLocalExitGet env nls >>= \ case
    FuncallExitReturn            ->
      pure $ EmacsSuccess res
    FuncallExitSignal (sym, dat) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      emacsSignalInfo <- extractSignalInfo env sym dat
      pure $ EmacsExitSignal $ EmacsSignal
        { emacsSignalSym    = toUnknown sym
        , emacsSignalData   = dat
        , emacsSignalOrigin = callStack
        , emacsSignalInfo
        }
    FuncallExitThrow{} ->
      throwIO $ mkEmacsInternalError $
        "The operation should have never exited via throw:" <> line <> pretty errMsg

checkNonLocalExitFull
  :: WithCallStack
  => Env
  -> NonLocalState
  -> a
  -> IO (EmacsRes EmacsSignal EmacsThrow a)
checkNonLocalExitFull env nls res =
  nonLocalExitGet env nls >>= \ case
    FuncallExitReturn            ->
      pure $ EmacsSuccess res
    FuncallExitSignal (sym, dat) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      emacsSignalInfo <- extractSignalInfo env sym dat
      pure $ EmacsExitSignal $ EmacsSignal
        { emacsSignalSym    = toUnknown sym
        , emacsSignalData   = dat
        , emacsSignalOrigin = callStack
        , emacsSignalInfo
        }
      -- -- Important to clean up so that we can still call Emacs
      -- -- functions to make nil return value, etc
      -- Env.nonLocalExitClear env
    FuncallExitThrow (tag, value) -> do
      -- Important to clean up so that we can still call Emacs functions to make nil return value, etc
      Env.nonLocalExitClear env
      pure $ EmacsExitThrow $ EmacsThrow
        { emacsThrowTag    = tag
        , emacsThrowValue  = value
        , emacsThrowOrigin = callStack
        }

extractSignalInfo :: WithCallStack => Env -> RawValue p -> RawValue 'Regular -> IO Text
extractSignalInfo env sym dat = do
  cons          <- reifySymbolUnknown env Sym.cons
  dat'          <- withPtrLenNonNull (foldMap PtrBuilder.storable $ Tuple2 (toUnknown sym, toUnknown dat)) $ \n args ->
    Env.funcallPrimitive env cons (fromIntegral n) args
  prin1ToString <- reifySymbolUnknown env Sym.prin1ToString
  formatted     <- withPtrLenNonNull (foldMap PtrBuilder.storable $ Tuple1 dat') $ \n args ->
    Env.funcallPrimitive env prin1ToString (fromIntegral n) args
  formatRes     <- unpackEnumFuncallExit =<< Env.nonLocalExitCheck env
  case formatRes of
    FuncallExitSignal{} -> do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError "Failed to format Emacs signal data"
    FuncallExitThrow{}  -> do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError "Failed to format Emacs signal data"
    FuncallExitReturn   ->
      extractTextUnsafe env formatted

extractTextUnsafe
  :: WithCallStack
  => Env
  -> RawValue p
  -> IO Text
extractTextUnsafe env x = TE.decodeUtf8With TE.lenientDecode <$> extractStringUnsafe env x

extractStringUnsafe
  :: WithCallStack
  => Env
  -> RawValue p
  -> IO BS.ByteString
extractStringUnsafe env x = do
  allocaNonNull $ \pSize -> do
    res <- Env.copyStringContents env x nullPtr pSize
    if Env.isNonTruthy res
    then do
      Env.nonLocalExitClear env
      throwIO $ mkEmacsInternalError
        "Failed to obtain size when unpacking string. Probable cause: emacs object is not a string."
    else do
      size          <- fromIntegral <$> peek (unNonNullPtr pSize)
      fp            <- BSI.mallocByteString size
      copyPerformed <- unsafeWithForeignPtr fp $ \ptr ->
        Env.copyStringContents env x (castPtr ptr) pSize
      if Env.isTruthy copyPerformed
      then
        -- Should subtract 1 from size to avoid NULL terminator at the end.
        pure $ BSI.BS fp (size - 1)
      else do
        Env.nonLocalExitClear env
        throwIO $ mkEmacsInternalError "Failed to unpack string"

data NonLocalState = NonLocalState
  { nlsErr  :: {-# UNPACK #-} !(NonNullPtr (RawValue 'Regular))
  , nlsData :: {-# UNPACK #-} !(NonNullPtr (RawValue 'Regular))
  }

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
    Env.nonLocalExitCheck env >>= unpackEnumFuncallExit >>= putMVar out
  NonLocalExitGet out   ->
    putMVar out =<< nonLocalExitGet env nls
  NonLocalExitSignal sym dat out -> do
    listSym <- reifySymbolUnknown env Sym.list
    withPtrLenNonNull dat $ \n args -> do
      dat'            <- Env.funcallPrimitive env listSym (fromIntegral n) args
      emacsSignalInfo <- extractSignalInfo env sym dat'
      Env.nonLocalExitSignal env sym dat'
      putMVar out EmacsSignal
        { emacsSignalSym    = toUnknown sym
        , emacsSignalData   = dat'
        , emacsSignalOrigin = callStack
        , emacsSignalInfo
        }

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
    putMVar out =<< extractString env nls x

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
