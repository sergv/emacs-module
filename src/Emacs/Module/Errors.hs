----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Errors
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- This module defines various kinds of exception that this library
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module Emacs.Module.Errors
  ( EmacsThrow(..)
  , reportEmacsThrowToEmacs
  , UserError(..)
  , mkUserError
  , EmacsError(..)
  , mkEmacsError
  , reportErrorToEmacs
  , EmacsInternalError(..)
  , mkEmacsInternalError
  , reportInternalErrorToEmacs

  , formatSomeException
  , reportAnyErrorToEmacs
  , reportAllErrorsToEmacs
  ) where

import Control.Applicative
import Control.Exception as Exception
import Control.Exception.Safe.Checked (Throws)
import Control.Exception.Safe.Checked qualified as Checked

import Data.ByteString.Char8 qualified as C8
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Void
import Data.Void.Unsafe
import Foreign
import Foreign.C.String
import GHC.Stack (CallStack, callStack, prettyCallStack)
import Prettyprinter
import Prettyprinter.Render.Text as PP

import Data.Emacs.Module.Env qualified as Raw
import Data.Emacs.Module.GetRawValue
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.Raw.Env.Internal (Env)
import Data.Emacs.Module.Raw.Value
import Data.Emacs.Module.SymbolName.Internal
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Emacs.Module.Assert

-- | A Haskell exception used to signal a @throw@ exit performed by an
-- Emacs function.
--
-- Unlikely to be needed when developing Emacs extensions.
data EmacsThrow = EmacsThrow
  { emacsThrowTag   :: !RawValue
  , emacsThrowValue :: !RawValue
  }

instance Show EmacsThrow where
  showsPrec _ _ = showString "EmacsThrow"

instance Exception EmacsThrow

reportEmacsThrowToEmacs :: Env -> EmacsThrow -> IO RawValue
reportEmacsThrowToEmacs env et = do
  nil <- mkNil env
  reportEmacsThrowToEmacs' env et
  pure nil

reportEmacsThrowToEmacs' :: Env -> EmacsThrow -> IO ()
reportEmacsThrowToEmacs' env EmacsThrow{emacsThrowTag, emacsThrowValue} = do
  Raw.nonLocalExitThrow env emacsThrowTag emacsThrowValue

-- | Error thrown to emacs by Haskell functions when anything goes awry.
data UserError = UserError
  { userErrFunctionName :: Doc Void
  , userErrMsg          :: Doc Void
  , userErrStack        :: CallStack
  } deriving (Show)

instance Exception UserError

instance Pretty UserError where
  pretty (UserError func msg stack) =
    "Error in function" <+> unsafeVacuous func <> ":" <> line <>
    indent 2 (unsafeVacuous msg) <> line <> line <>
    "Location:" <> line <>
    indent 2  (ppCallStack stack)

mkUserError
  :: WithCallStack
  => Doc Void -- ^ Function name
  -> Doc Void -- ^ Message body
  -> UserError
mkUserError funcName body = UserError
  { userErrFunctionName = funcName
  , userErrMsg          = body
  , userErrStack        = callStack
  }

-- | A high-level error thrown when an Emacs function fails.
data EmacsError = EmacsError
  { emacsErrMsg    :: Doc Void
  , emacsErrData   :: Doc Void
  , emacsErrStack  :: CallStack
  } deriving (Show)

instance Exception EmacsError

mkEmacsError
  :: WithCallStack
  => Doc Void -- ^ Message
  -> Doc Void -- ^ Error data from Emacs
  -> EmacsError
mkEmacsError msg errData = EmacsError
  { emacsErrMsg   = msg
  , emacsErrData  = errData
  , emacsErrStack = callStack
  }

instance Pretty EmacsError where
  pretty EmacsError{emacsErrMsg, emacsErrData, emacsErrStack} =
    "Error within Haskell<->Emacs bindings:" <> line <>
    indent 2 (unsafeVacuous emacsErrMsg) <> line <> line <>
    "Emacs error:" <> line <>
    indent 2 (unsafeVacuous emacsErrData) <> line <> line <>
    "Location:" <> line <>
    indent 2 (ppCallStack emacsErrStack)

reportErrorToEmacs :: Env -> EmacsError -> IO RawValue
reportErrorToEmacs env e = do
  nil <- mkNil env
  report render env e
  pure nil

-- | A low-level error thrown when assumptions of this package are
-- violated and it's not safe to proceed further.
data EmacsInternalError = EmacsInternalError
  { emacsInternalErrMsg   :: Doc Void
  , emacsInternalErrStack :: CallStack
  } deriving (Show)

instance Exception EmacsInternalError

mkEmacsInternalError
  :: WithCallStack
  => Doc Void -- ^ Error message
  -> EmacsInternalError
mkEmacsInternalError msg = EmacsInternalError
  { emacsInternalErrMsg   = msg
  , emacsInternalErrStack = callStack
  }

reportInternalErrorToEmacs :: Env -> EmacsInternalError -> IO RawValue
reportInternalErrorToEmacs env e = do
  nil <- mkNil env
  report render env e
  pure nil

instance Pretty EmacsInternalError where
  pretty EmacsInternalError{emacsInternalErrMsg, emacsInternalErrStack} =
    "Internal error within Haskell<->Emacs bindings:" <> line <>
    indent 2 (unsafeVacuous emacsInternalErrMsg) <> line <> line <>
    "Location:" <> line <>
    indent 2 (ppCallStack emacsInternalErrStack)

formatSomeException :: SomeException -> Text
formatSomeException e =
  case pretty @EmacsError         <$> fromException e <|>
       pretty @EmacsInternalError <$> fromException e <|>
       pretty @UserError          <$> fromException e of
    Just formatted -> render' formatted
    Nothing ->
      PP.renderStrict $ layoutPretty defaultLayoutOptions $
        "Error within Haskell<->Emacs bindings:" <> line <>
        indent 2 (pretty (show e))

reportAnyErrorToEmacs :: Env -> SomeException -> IO RawValue
reportAnyErrorToEmacs env e = do
  nil <- mkNil env
  report formatSomeException env e
  pure nil

-- | Catch all errors this package might throw in an IO action
-- and make Emacs aware of them.
--
-- This is a convenience function intended to be used around exported
-- @initialise@ entry point into an Emacs module.
reportAllErrorsToEmacs
  :: Env
  -> IO a -- ^ Result to return on error.
  -> ((Throws EmacsInternalError, Throws EmacsError, Throws UserError, Throws EmacsThrow) => IO a)
  -> IO a
reportAllErrorsToEmacs env resultOnErr x
  = Exception.handle (\e -> report formatSomeException env e *> resultOnErr)
  $ Checked.handle (\et -> reportEmacsThrowToEmacs' env et *> resultOnErr)
  $ Checked.uncheck (Proxy @EmacsInternalError)
  $ Checked.uncheck (Proxy @EmacsError)
  $ Checked.uncheck (Proxy @UserError) x

report :: (e -> Text) -> Env -> e -> IO ()
report format env err = do
  errSym  <- reifySymbolRaw env Sym.error
  listSym <- reifySymbolRaw env Sym.list
  withTextAsCString0AndLen (format err) $ \str len -> do
    str' <- Raw.makeString env str (fromIntegral len)
    alloca $ \argsPtr -> do
      poke argsPtr str'
      errData <- Raw.funcallPrimitive env (getRawValue listSym) 1 (mkNonNullPtr argsPtr)
      -- The 'nonLocalExitSignal' function does not overwrite pending
      -- signals, so it's ok to use it here without checking whether an
      -- error is already going on.
      Raw.nonLocalExitSignal env errSym errData

withTextAsCString0AndLen :: Text -> (CString -> Int -> IO a) -> IO a
withTextAsCString0AndLen str f =
  C8.useAsCString utf8 (\ptr -> f ptr (C8.length utf8))
  where
    utf8 = TE.encodeUtf8 str

mkNil :: WithCallStack => Env -> IO RawValue
mkNil env =
  reifySymbolRaw env Sym.nil

render :: Pretty a => a -> Text
render = render' . pretty

render' :: Doc Void -> Text
render' = PP.renderStrict . layoutPretty defaultLayoutOptions

ppCallStack :: CallStack -> Doc ann
ppCallStack = pretty . prettyCallStack

