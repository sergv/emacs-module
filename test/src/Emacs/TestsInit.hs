-- |
-- Module      :  Emacs.TestsInit
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Emacs.TestsInit () where

import Debug.Trace

import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as C8
import Data.Maybe
import Foreign
#if MIN_VERSION_base(4,10,0)
import Foreign.C
#endif

import Data.Emacs.Module.Args
import Data.Emacs.Module.Runtime (Runtime)
import qualified Data.Emacs.Module.Runtime as Runtime
import Data.Emacs.Module.SymbolName.TH
import qualified Data.Emacs.Module.Value as Emacs
import Emacs.Module
import Emacs.Module.Assert

#if MIN_VERSION_base(4,10,0)
foreign export ccall initialise :: Ptr Runtime -> IO CBool

true, false :: CBool
true  = CBool 1
false = CBool 0

#else
foreign export ccall initialise :: Ptr Runtime -> IO Int

true, false :: Int
true  = 1
false = 0
#endif

#if MIN_VERSION_base(4,10,0)
initialise :: WithCallStack => Ptr Runtime -> IO CBool
#else
initialise :: WithCallStack => Ptr Runtime -> IO Int
#endif
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
  bindFunction [esym|haskell-emacs-module-tests-apply2|] =<<
    makeFunction apply2 "Apply a function twice."
  bindFunction [esym|haskell-emacs-module-tests-add|] =<<
    makeFunction add "Add two numbers."
  bindFunction [esym|haskell-emacs-module-tests-get-rest|] =<<
    makeFunction getRest "Just return the &rest argument."
  bindFunction [esym|haskell-emacs-module-tests-append-lots-of-strings|] =<<
    makeFunction appendLotsOfStrings "Append foo string N times to itself."
  pure True

apply2
  :: WithCallStack
  => EmacsFunction ('S ('S 'Z)) 'Z 'False
apply2 env f x = runEmacsM env $ do
  y <- funcallPrimitive [esym|funcall|] [f, x]
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

appendLotsOfStrings
  :: WithCallStack
  => EmacsFunction ('S 'Z) 'Z 'False
appendLotsOfStrings env n = runEmacsM env $ do
  n'     <- extractInt n
  foo'   <- makeString "foo"
  empty' <- makeString ""
  let input = replicate n' (pure foo', "foo")
      res = appendTree concat2' input
  traceM $ "input = " ++ show (map snd input)
  res' <- traverse fst res
  pure $ fromMaybe empty' res'

concat2'
  :: (WithCallStack, MonadEmacs m)
  => (m Emacs.RawValue, C8.ByteString)
  -> (m Emacs.RawValue, C8.ByteString)
  -> (m Emacs.RawValue, C8.ByteString)
concat2' (x, xStr) (y, yStr) =
  (go, xStr <> yStr)
  where
    go = do
      x' <- x
      y' <- y
      funcallPrimitive [esym|garbage-collect|] []
      traceM $ "xStr = " ++ show (C8.length xStr) ++ ", yStr = " ++ show (C8.length yStr)
      concat2 x' y'

appendTree :: WithCallStack => (a -> a -> a) -> [a] -> Maybe a
appendTree f = reduce
  where
    go []             = []
    go xs@[_]         = xs
    go (x1 : x2 : xs) = f x1 x2 : go xs

    reduce []  = Nothing
    reduce [x] = Just x
    reduce xs  = reduce (go xs)

