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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Emacs.TestsInit () where

import Data.ByteString.Char8 qualified as C8
import Data.Functor
import Data.Maybe
import Foreign hiding (void)
import Foreign.C

import Data.Emacs.Module.Args
import Data.Emacs.Module.Runtime (Runtime)
import Data.Emacs.Module.Runtime qualified as Runtime
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Monad.Async qualified as Async

foreign export ccall initialise :: Ptr Runtime -> IO CBool

true, false :: CBool
true  = CBool 1
false = CBool 0

initialise :: WithCallStack => Ptr Runtime -> IO CBool
initialise runtime = do
  runtime' <- Runtime.validateRuntime runtime
  case runtime' of
    Nothing        -> pure false
    Just runtime'' ->
      Runtime.withEnvironment runtime'' $ \env -> do
        res <- reportAllErrorsToEmacs env (pure False) $ Async.runEmacsM env initialise'
        pure $ if res then true else false

initialise'
  :: (WithCallStack, MonadEmacs m v)
  => m s Bool
initialise' = do
  bindFunction "haskell-emacs-module-tests-apply2" =<<
    makeFunction apply2 "Apply a function twice."
  bindFunction "haskell-emacs-module-tests-add" =<<
    makeFunction add "Add two numbers."
  bindFunction "haskell-emacs-module-tests-get-rest" =<<
    makeFunction getRest "Just return the &rest argument."
  bindFunction "haskell-emacs-module-tests-append-lots-of-strings" =<<
    makeFunction appendLotsOfStrings "Append foo string N times to itself."
  bindFunction "haskell-emacs-module-tests-append-lots-of-vectors" =<<
    makeFunction appendLotsOfVectors "Append [1 2 3] vector N times to itself."
  bindFunction "haskell-emacs-module-tests-replicate" =<<
    makeFunction emacsReplicate "Replicate an item N times"
  bindFunction "haskell-emacs-module-tests-grow-list" =<<
    makeFunction emacsGrowList "Append list with itself"
  bindFunction "haskell-emacs-module-tests-incorrect-vector-assignment" =<<
    makeFunction emacsIncorrectVectorAssignment "vecSet that should result in error"
  pure True

apply2
  :: (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S ('S 'Z)) 'Z 'False m v s
apply2 (R f (R x Stop)) = do
  -- funcallSym <- intern Sym.funcall
  -- y          <- funcall funcallSym [f, x]
  -- res        <- funcall funcallSym [f, y]
  y          <- funcall f [x]
  res        <- funcall f [y]
  pure res

add
  :: (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S ('S 'Z)) 'Z 'False m v s
add (R x (R y Stop)) =
  makeInt =<< (+) <$> extractInt x <*> extractInt y

getRest
  :: (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S 'Z) 'Z 'True m v s
getRest (R _req (Rest rest)) = do
  vectorSym <- intern Sym.vector
  funcall vectorSym rest

appendLotsOfStrings
  :: forall m v s. (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S 'Z) 'Z 'False m v s
appendLotsOfStrings (R n Stop) = do
  n'     <- extractInt n
  -- foo'   <- makeString "foo"
  empty' <- makeString ""
  let input :: [(m s (v s), C8.ByteString)]
      input = replicate n' (makeString "foo", "foo")
      res = appendTree concat2' input
  res' <- traverse fst res
  pure $ fromMaybe empty' res'

appendLotsOfVectors
  :: (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S 'Z) 'Z 'False m v s
appendLotsOfVectors (R n Stop) = do
  n'     <- extractInt n
  one    <- makeInt 1
  two    <- makeInt 2
  three  <- makeInt 3

  empty' <- makeVector []

  let input = replicate n' (makeVector [one, two, three], [1, 2, 3])
      res   = appendTree vconcat2' input
  res' <- traverse fst res
  pure $ fromMaybe empty' res'

emacsReplicate
  :: (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S ('S 'Z)) 'Z 'False m v s
emacsReplicate (R n (R x Stop)) = do
  n' <- extractInt n
  makeList (replicate n' x)

emacsGrowList
  :: (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S 'Z) 'Z 'False m v s
emacsGrowList (R xs Stop) = do
  ys <- extractList xs
  makeList $ ys ++ ys ++ ys

emacsIncorrectVectorAssignment
  :: (WithCallStack, MonadEmacs m v)
  => EmacsFunction 'Z 'Z 'False m v s
emacsIncorrectVectorAssignment Stop = do
  n   <- makeInt 36
  vec <- makeVector $ replicate 42 n
  vecSet vec 42 n
  nil

concat2'
  :: (WithCallStack, MonadEmacs m v)
  => (m s (v s), C8.ByteString)
  -> (m s (v s), C8.ByteString)
  -> (m s (v s), C8.ByteString)
concat2' (x, xStr) (y, yStr) =
  (go, xStr <> yStr)
  where
    go = do
      x'    <- x
      y'    <- y
      gcSym <- intern "garbage-collect"
      void $ funcallPrimitiveUnchecked gcSym []
      concat2 x' y'

vconcat2'
  :: (WithCallStack, MonadEmacs m v)
  => (m s (v s), [Int])
  -> (m s (v s), [Int])
  -> (m s (v s), [Int])
vconcat2' (x, xs) (y, ys) =
  (go, xs <> ys)
  where
    go = do
      x'    <- x
      y'    <- y
      gcSym <- intern "garbage-collect"
      void $ funcallPrimitiveUnchecked gcSym []
      vconcat2 x' y'

appendTree :: (a -> a -> a) -> [a] -> Maybe a
appendTree f = reduce
  where
    go []             = []
    go xs@[_]         = xs
    go (x1 : x2 : xs) = f x1 x2 : go xs

    reduce []  = Nothing
    reduce [x] = Just x
    reduce xs  = reduce (go xs)
