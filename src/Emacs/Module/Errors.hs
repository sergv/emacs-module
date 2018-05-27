----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Module.Errors
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- This module defines various kinds of exception that this library
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

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
import qualified Control.Exception.Safe.Checked as Checked

import qualified Data.ByteString.Char8 as C8
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
import Data.Void
import Data.Void.Unsafe
import Foreign.C.String
import Foreign.Marshal.Array
import GHC.Stack (CallStack, callStack, prettyCallStack)
import Text.Show (showString)

import qualified Data.Emacs.Module.Env as Raw
import Data.Emacs.Module.Env.Internal (Env)
import Data.Emacs.Module.NonNullPtr
import Data.Emacs.Module.SymbolName (useSymbolNameAsCString)
import Data.Emacs.Module.SymbolName.TH
import qualified Data.Emacs.Module.Value as Emacs
import Emacs.Module.Assert

-- | A Haskell exception used to signal a @throw@ exit performed by an
-- Emacs function.
--
-- Unlikely to be needed when developing Emacs extensions.
data EmacsThrow = EmacsThrow
  { emacsThrowTag    :: !Emacs.Value
  , emacsThrowValue  :: !Emacs.Value
  }

instance Show EmacsThrow where
  showsPrec _ _ = showString "EmacsThrow"

instance Exception EmacsThrow

reportEmacsThrowToEmacs :: Env -> EmacsThrow -> IO Emacs.Value
reportEmacsThrowToEmacs env EmacsThrow{emacsThrowTag, emacsThrowValue} = do
  Raw.nonLocalExitThrow env emacsThrowTag emacsThrowValue
  returnNil env

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

reportErrorToEmacs :: Env -> EmacsError -> IO Emacs.Value
reportErrorToEmacs env e = do
  report render env e
  returnNil env

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

reportInternalErrorToEmacs :: Env -> EmacsInternalError -> IO Emacs.Value
reportInternalErrorToEmacs env e = do
  report render env e
  returnNil env

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

reportAnyErrorToEmacs :: Env -> SomeException -> IO Emacs.Value
reportAnyErrorToEmacs env e = do
  report formatSomeException env e
  returnNil env

-- | Catch all errors this package might throw in an IO action
-- and make Emacs aware of them.
--
-- This is a convenience function intended to be used around exported
-- @initialise@ entry point into an Emacs module.
reportAllErrorsToEmacs
  :: Env
  -> IO a --
  -> ((Throws EmacsInternalError, Throws EmacsError, Throws UserError, Throws EmacsThrow) => IO a)
  -> IO a
reportAllErrorsToEmacs env resultOnErr x =
  Exception.handle (\e -> report formatSomeException env e *> resultOnErr) $
  Checked.uncheck (Proxy @EmacsInternalError) $
  Checked.uncheck (Proxy @EmacsError) $
  Checked.uncheck (Proxy @UserError) $
  Checked.uncheck (Proxy @EmacsThrow) x

report :: (e -> Text) -> Env -> e -> IO ()
report format env err = do
  errSym  <- useSymbolNameAsCString [esym|error|] (Raw.intern env)
  listSym <- useSymbolNameAsCString [esym|list|] (Raw.intern env)
  withTextAsCStringLen (format err) $ \(str, len) -> do
    str' <- Raw.makeString env str (fromIntegral len)
    withArrayLen [str'] $ \nargs argsPtr -> do
      errData <- Raw.funcall env listSym (fromIntegral nargs) (mkNonNullPtr argsPtr)
      -- The 'nonLocalExitSignal' function does not overwrite pending
      -- signals, so it's ok to use it here without checking whether an
      -- error is already going on.
      Raw.nonLocalExitSignal env errSym errData

withTextAsCStringLen :: Text -> (CStringLen -> IO a) -> IO a
withTextAsCStringLen str = C8.useAsCStringLen (TE.encodeUtf8 str)

returnNil :: Env -> IO Emacs.Value
returnNil env =
  useSymbolNameAsCString [esym|nil|] (Raw.intern env)


render :: Pretty a => a -> Text
render = render' . pretty

render' :: Doc Void -> Text
render' = PP.renderStrict . layoutPretty defaultLayoutOptions

ppCallStack :: CallStack -> Doc ann
ppCallStack = pretty . prettyCallStack

