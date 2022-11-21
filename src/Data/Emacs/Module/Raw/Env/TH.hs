----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Env.TH
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Emacs.Module.Raw.Env.TH (wrapEmacsFunc, Safety(..)) where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.List (foldl')
import Foreign.Ptr as Foreign
import Language.Haskell.TH

import Data.Emacs.Module.Raw.Env.Internal as Env

decomposeFunctionType :: Type -> ([Type], Type)
decomposeFunctionType = go []
  where
    go :: [Type] -> Type -> ([Type], Type)
    go args = \case
      ForallT _ _ t          -> go args t
      AppT (AppT ArrowT x) y -> go (x : args) y
      ret                    -> (reverse args, ret)

#if MIN_VERSION_template_haskell(2, 17, 0)
unwrapForall :: Type -> (Maybe ([TyVarBndr Specificity], Cxt), Type)
#else
unwrapForall :: Type -> (Maybe ([TyVarBndr], Cxt), Type)
#endif
unwrapForall (ForallT bs c t) = (Just (bs, c), t)
unwrapForall t                = (Nothing, t)

#if MIN_VERSION_template_haskell(2, 17, 0)
wrapForall :: Maybe ([TyVarBndr Specificity], Cxt) -> Type -> Type
#else
wrapForall :: Maybe ([TyVarBndr], Cxt) -> Type -> Type
#endif
wrapForall Nothing        = id
wrapForall (Just (bs, c)) = ForallT bs c

wrapEmacsFunc :: String -> Safety -> ExpQ -> TypeQ -> DecsQ
wrapEmacsFunc name safety peekExpr rawFuncType = do
  rawFuncType' <- rawFuncType
  let (forallCxt, rawFuncType'') = unwrapForall rawFuncType'
      (args, ret)                = decomposeFunctionType rawFuncType''
  (envArg, otherArgs) <- case args of
    [] -> fail $
      "Raw function type must take at least one emacs_env argument: " ++ show rawFuncType'
    x : xs
     | x /= ConT ''Env.Env -> fail $
       "Raw function type must take emacs_env as a first argument, but takes " ++ show x ++ " in " ++ show rawFuncType'
     | otherwise ->
        (,) <$> newName "env" <*> traverse (const (newName "x")) xs
  foreignFuncName <- newName $ "emacs_func_" ++ name
  let envPat :: PatQ
      envPat = varP envArg
      pats   = envPat : map varP otherArgs
      body   = normalB $ do
        funPtrVar <- newName "funPtr"
        [e|liftIO|] `appE` doE
          [ bindS (varP funPtrVar) $ peekExpr `appE` ([e| Env.toPtr |] `appE` varE envArg)
          , noBindS $ foldl' appE (varE foreignFuncName) (map varE $ funPtrVar : envArg : otherArgs)
          ]
  m    <- newName "m"
  ret' <- case ret of
    AppT monad result
      | monad == ConT ''IO
      -> appT (varT m) (pure result)
    _ -> fail $ "Expected function that returns result in IO monad"
  let tv         = PlainTV m SpecifiedSpec
      constraint = ConT ''MonadIO `AppT` (VarT m)
  typeSig      <- sigD name' $ pure $
    wrapForall (Just (maybe ([tv], [constraint]) (bimap (tv :) (constraint :)) forallCxt)) $
      foldr (\x acc -> ArrowT `AppT` x `AppT` acc) ret' args
  mainDecl     <- funD name' [clause pats body []]
  inlinePragma <- pragInlD name' Inline FunLike AllPhases
  let foreignDeclType :: TypeQ
      foreignDeclType =
        fmap (wrapForall forallCxt) $
        arrowT `appT` (conT ''Foreign.FunPtr `appT` pure rawFuncType'') `appT` pure rawFuncType''
  foreignDecl <- forImpD cCall safety "dynamic" foreignFuncName foreignDeclType
  pure [typeSig, mainDecl, inlinePragma, foreignDecl]
  where
    name' = mkName name

