-- |
-- Module      :  Emacs.TestsInit
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Emacs.TestsInit () where

import Data.ByteString.Char8 qualified as C8
import Data.Maybe
import Foreign
import Foreign.C

import Data.Emacs.Module.Args
import Data.Emacs.Module.Runtime (Runtime)
import Data.Emacs.Module.Runtime qualified as Runtime
import Data.Emacs.Module.SymbolName
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

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

initialise'
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s Bool
initialise' = do
  bindFunction (mkSymbolNameUnsafe# "haskell-emacs-module-tests-apply2"#) =<<
    makeFunction apply2 "Apply a function twice."
  bindFunction (mkSymbolNameUnsafe# "haskell-emacs-module-tests-add"#) =<<
    makeFunction add "Add two numbers."
  bindFunction (mkSymbolNameUnsafe# "haskell-emacs-module-tests-get-rest"#) =<<
    makeFunction getRest "Just return the &rest argument."
  bindFunction (mkSymbolNameUnsafe# "haskell-emacs-module-tests-append-lots-of-strings"#) =<<
    makeFunction appendLotsOfStrings "Append foo string N times to itself."
  bindFunction (mkSymbolNameUnsafe# "haskell-emacs-module-tests-append-lots-of-vectors"#) =<<
    makeFunction appendLotsOfVectors "Append [1 2 3] vector N times to itself."
  bindFunction (mkSymbolNameUnsafe# "haskell-emacs-module-tests-replicate"#) =<<
    makeFunction emacsReplicate "Replicate an item N times"
  pure True

apply2
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
apply2 (R f (R x Stop)) = do
  y <- funcallPrimitive (mkSymbolNameUnsafe# "funcall"#) [f, x]
  produceRef =<< funcall (mkSymbolNameUnsafe# "funcall"#) [f, y]

add
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
add (R x (R y Stop)) =
  produceRef =<< makeInt =<< (+) <$> extractInt x <*> extractInt y

getRest
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsFunction ('S 'Z) 'Z 'True s m
getRest (R _req (Rest rest)) =
  produceRef =<< funcall (mkSymbolNameUnsafe# "vector"#) rest

appendLotsOfStrings
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s), MonadMask (m s))
  => EmacsFunction ('S 'Z) 'Z 'False s m
appendLotsOfStrings (R n Stop) = do
  n'     <- extractInt n
  -- foo'   <- makeString "foo"
  empty' <- makeString ""
  let input :: [(m s (EmacsRef m s), C8.ByteString)]
      input = replicate n' (makeString "foo", "foo")
      res = appendTree concat2' input
  res' <- traverse fst res
  produceRef $ fromMaybe empty' res'

appendLotsOfVectors
  :: (WithCallStack, MonadEmacs m, Monad (m s), MonadMask (m s))
  => EmacsFunction ('S 'Z) 'Z 'False s m
appendLotsOfVectors (R n Stop) = do
  n'     <- extractInt n
  one    <- makeInt 1
  two    <- makeInt 2
  three  <- makeInt 3

  empty' <- makeVector []

  let input = replicate n' (makeVector [one, two, three], [1, 2, 3])
      res = appendTree vconcat2' input
  res' <- traverse fst res
  produceRef $ fromMaybe empty' res'

emacsReplicate
  :: (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
emacsReplicate (R n (R x Stop)) = do
  n' <- extractInt n
  produceRef =<< makeList (replicate n' x)

concat2'
  :: (WithCallStack, MonadEmacs m, MonadMask (m s))
  => (m s (EmacsRef m s), C8.ByteString)
  -> (m s (EmacsRef m s), C8.ByteString)
  -> (m s (EmacsRef m s), C8.ByteString)
concat2' (x, xStr) (y, yStr) =
  (go, xStr <> yStr)
  where
    go = do
      x' <- x
      y' <- y
      withCleanup x' $ \x'' ->
        withCleanup y' $ \y'' -> do
          _ <- funcallPrimitive (mkSymbolNameUnsafe# "garbage-collect"#) []
          concat2 x'' y''

vconcat2'
  :: (WithCallStack, MonadEmacs m, MonadMask (m s))
  => (m s (EmacsRef m s), [Int])
  -> (m s (EmacsRef m s), [Int])
  -> (m s (EmacsRef m s), [Int])
vconcat2' (x, xs) (y, ys) =
  (go, xs <> ys)
  where
    go = do
      x' <- x
      y' <- y
      withCleanup x' $ \x'' ->
        withCleanup y' $ \y'' -> do
          _ <- funcallPrimitive (mkSymbolNameUnsafe# "garbage-collect"#) []
          vconcat2 x'' y''

appendTree :: (a -> a -> a) -> [a] -> Maybe a
appendTree f = reduce
  where
    go []             = []
    go xs@[_]         = xs
    go (x1 : x2 : xs) = f x1 x2 : go xs

    reduce []  = Nothing
    reduce [x] = Just x
    reduce xs  = reduce (go xs)
