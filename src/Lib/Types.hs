{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Types
where

import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Subst
import Lib.Types.Unify
import Lib.Types.Pred
import Lib.Types.Scheme
import Lib.Types.Infer
import Lib.Types.Assump
import Lib.Types.TIMonad

import Lib.Syntax
import Lib.Parser (parse_multi, parse_expr)

import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Text (Text, pack)

easy_parse :: MonadIO m => String -> m (CoreExpr ())
easy_parse str = do
    Just e <- parse_expr . pack $ str
    return e

easy_parse_defn :: MonadIO m => String -> m [(Symbol, CoreExpr ())]
easy_parse_defn str = do
    defns <- parse_multi . pack $ str
    return $ map (\(Free (DefC sym val)) -> (sym, val)) defns

type Alt = (Symbol, Expr)
type Expl = (Symbol, Scheme, [Alt])
type Impl = (Symbol, [Alt])

type BindGroup = ([Expl], [Impl])

data Expr
    = Var Symbol
    | Ap Expr Expr
    | Lam Symbol Expr
    | If Expr Expr Expr
    | Const Assump
    | Let BindGroup Expr
    deriving (Show)

-- | Convert the parsed syntax to the syntax the magical stolen code will be
-- able to use correctly.
toExpr :: CoreExpr () -> Expr
toExpr (Free (IdC sym)) = Var sym

toExpr (Free (IfC c t e)) = If (toExpr c)
                               (toExpr t)
                               (toExpr e)

toExpr (Free (IntC n)) = Const ((show n) :>: (toScheme tInteger))

toExpr (Free (DoubleC n)) = Const ((show n) :>: (toScheme tFloat))

toExpr (Free (BoolC b)) = Const (b' :>: (toScheme tBoolean))
    where b' = if b then "#t" else "#f"

toExpr (Free (AppC fun args)) = foldl Ap (toExpr fun) $
                                fmap toExpr args

toExpr (Free (FunC args body)) = foldr Lam (toExpr body) args

defaultAssumptions :: [Assump]
defaultAssumptions =
    [ "+" :>: (Forall [Star] $
               [IsIn "Num" [TGen 0]] :=>
                  (TGen 0 `fn` TGen 0 `fn` TGen 0))
    , "*" :>: (Forall [Star] $
               [IsIn "Num" [TGen 0]] :=>
                  (TGen 0 `fn` TGen 0 `fn` TGen 0))
    ]

tiExpr :: Infer Expr Type
tiExpr ce as (Var i) = do
    sc <- find i as
    (ps :=> t) <- freshInst sc
    return (ps, t)

tiExpr ce as (Const (i :>: sc)) = do
    (ps :=> t) <- freshInst sc
    return (ps, t)

tiExpr ce as (If c t e) = do
    (ps, c') <- tiExpr ce as c
    unify c' tBoolean
    (ps1, t') <- tiExpr ce as t
    (ps2, e') <- tiExpr ce as e
    unify t' e'
    return (ps++ps1++ps2, t')

tiExpr ce as (Ap e f) = do
    (ps, te) <- tiExpr ce as e
    (qs, tf) <- tiExpr ce as f
    t <- newTVar Star
    unify (tf `fn` t) te
    return (ps++qs, t)

tiExpr ce as (Lam sym expr) = do
    v <- newTVar Star
    let as' = sym :>: (toScheme v)
    (qs, ts) <- tiExpr ce (as':as) expr
    return (qs, fn v ts)
