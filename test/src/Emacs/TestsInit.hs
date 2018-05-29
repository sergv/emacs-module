-- |
-- Module      :  Emacs.TestsInit
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Emacs.TestsInit () where

import Control.Monad.IO.Class

import Foreign
import Foreign.C

import Data.Emacs.Module.Args
import Data.Emacs.Module.Runtime (Runtime)
import qualified Data.Emacs.Module.Runtime as Runtime
import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert

foreign export ccall initialise :: Ptr Runtime -> IO CBool

true, false :: CBool
true  = CBool 1
false = CBool 0

initialise :: WithCallStack => Ptr Runtime -> IO CBool
initialise runtime = do
  runtime' <- Runtime.validateRuntime runtime
  case runtime' of
    Nothing        -> pure false
    Just runtime'' -> do
      env <- Runtime.getEnvironment runtime''
      res <- reportAllErrorsToEmacs env (pure False) $ runEmacsM env initialise'
      pure $ if res then true else false

initialise' :: (WithCallStack, MonadEmacs m, MonadIO m) => m Bool
initialise' = do
  funApply2 <- makeFunction apply2 "Apply a function twice."
  bindFunction [esym|haskell-emacs-module-tests-apply2|] funApply2
  funAdd <- makeFunction add "Add two numbers."
  bindFunction [esym|haskell-emacs-module-tests-add|] funAdd
  funGetRest <- makeFunction getRest "Just return the &rest argument."
  bindFunction [esym|haskell-emacs-module-tests-get-rest|] funGetRest
  pure True

apply2
  :: WithCallStack
  => EmacsFunction ('S ('S 'Z)) 'Z 'False
apply2 env f x = runEmacsM env $ do
  y <- funcall [esym|funcall|] [f, x]
  funcall [esym|funcall|] [f, y]

add
  :: WithCallStack
  => EmacsFunction ('S ('S 'Z)) 'Z 'False
add env x y = runEmacsM env $
  makeInt =<< (+) <$> extractInt x <*> extractInt y

getRest
  :: WithCallStack
  => EmacsFunction ('S 'Z) 'Z 'True
getRest env _req rest = runEmacsM env $
  funcall [esym|vector|] rest
