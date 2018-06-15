----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Module.Raw.Env.TH
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Emacs.Module.Raw.Env.TH (wrapEmacsFunc, Safety(..)) where

import Control.Monad.IO.Class
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

unwrapForall :: Type -> (Maybe ([TyVarBndr], Cxt), Type)
unwrapForall (ForallT bs c t) = (Just (bs, c), t)
unwrapForall t                = (Nothing, t)

wrapForall :: Maybe ([TyVarBndr], Cxt) -> Type -> Type
wrapForall Nothing        = id
wrapForall (Just (bs, c)) = ForallT bs c

--   AppT (AppT ArrowT x) ret -> go [] ret x
--   invalid                  -> fail $ "Invalid function type: " ++ show invalid
--   where
--     go :: [Type] -> Type -> Type -> Q ([Type], Type)
--     go args ret = \case
--       AppT ArrowT firstArg -> pure (firstArg : args, ret)
--       AppT x      y        -> go (y : args) ret x
--       invalid              -> fail $ "Invalid function type: " ++ show invalid

wrapEmacsFunc :: String -> Safety -> ExpQ -> TypeQ -> DecsQ
wrapEmacsFunc name safety peekExpr rawFuncType = do
  rawFuncType' <- rawFuncType
  let (forallCxt, rawFuncType'') = unwrapForall rawFuncType'
      (args, _ret)               = decomposeFunctionType rawFuncType''
  (envArg, otherArgs) <- case args of
    [] -> fail $
      "Raw function type must take at least one emacs_env argument: " ++ show rawFuncType'
    x : xs
     | x /= ConT ''Env.Env -> fail $
       "Raw function type must take emacs_env as a first argument, but takes " ++ show x ++ " in " ++ show rawFuncType'
     | otherwise ->
        (,) <$> newName "env" <*> traverse (const (newName "x")) xs
  foreignFuncName <- newName $ "emacs_func_" ++ name
  -- fail $ "otherArgs = " ++ show otherArgs ++ ", rawFuncType = " ++ show rawFuncType'
  let envPat = varP envArg
      pats   = envPat : map varP otherArgs
      body = normalB $ do
        funPtrVar <- newName "funPtr"
        [e|liftIO|] `appE` doE
          [ bindS (varP funPtrVar) $ peekExpr `appE` ([e| Env.toPtr |] `appE` varE envArg)
          , noBindS $ foldl' appE (varE foreignFuncName) (map varE $ funPtrVar : envArg : otherArgs)
          ]
  mainDecl     <- funD name' [clause pats body []]
  inlinePragma <- pragInlD name' Inline FunLike AllPhases
  let foreignDeclType =
        fmap (wrapForall forallCxt) $
        arrowT `appT` (conT ''Foreign.FunPtr `appT` pure rawFuncType'') `appT` pure rawFuncType''
  foreignDecl <- forImpD cCall safety "dynamic" foreignFuncName foreignDeclType
  pure [mainDecl, inlinePragma, foreignDecl]
  where
    name' = mkName name

