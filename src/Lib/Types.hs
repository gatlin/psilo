{-# LANGUAGE OverloadedStrings #-}

module Lib.Types
    ( typecheck_defn
    , typecheck_defns
    , TypeEnv(..)
--    , TypeError(..)
    , extendEnv
    , envLookup
    , defaultTypeEnv
    )
where

-- |
-- Inspiration for this code came from
--   https://brianmckenna.org/blog/type_annotation_cofree
--
-- I also liberally plagiarized this page:
--   http://dev.stephendiehl.com/fun/006_hindley_milner.html#generalization-and-instantiation
--
-- Finally, the paper "Typing Haskell in Haskell" was plagiarized once I
-- realized it's where the others were plagiarizing from.
--
-- It has been modified for a slightly different type and more flexible type
-- grammar.

import Control.Monad (forM_, forM, foldM, liftM2, zipWithM, mapAndUnzipM, guard)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Free
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid ((<>))

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import Lib.Syntax ( Symbol
                  , CoreExpr
                  , CoreAst(..)
                  , TopLevel(..)
                  , SurfaceExpr
                  , SurfaceAst(..)
                  , AnnotatedExpr
                  , annotated
                  )

import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Class
import Lib.Types.Frame
import Lib.Types.Qual
import Lib.Types.Constraint
import Lib.Types.PredMap
import Lib.Types.Scheme
import Lib.Types.TypeEnv
import Lib.Types.Infer
import Lib.Types.Solve

import Lib.Parser (parse_expr, parse_multi)
import Lib.Errors

-- * Defaults

num_binop :: Qual Type
num_binop = [IsIn "Num" t_0] :=> (TFun [t_0, t_0, t_0])
    where t_0 = TVar (TyVar 0 Star)

eq_binop :: Qual Type
eq_binop = [IsIn "Eq" t_0] :=> (TFun [t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

ord_binop :: Qual Type
ord_binop = [IsIn "Ord" t_0] :=> (TFun [t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

-- | Builtin operators and functions with explicit type schemes
defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv $ M.fromList
    [ ("*", generalize mempty num_binop)
    , ("+", generalize mempty num_binop)
    , ("-", generalize mempty num_binop)
    , ("/", generalize mempty num_binop)
    , ("=", generalize mempty eq_binop)
    , ("<", generalize mempty ord_binop)
    , (">", generalize mempty ord_binop)
    , ("id", generalize mempty $ [] :=> (TFun [TVar (TyVar 0 Star),
                                               TVar (TyVar 0 Star)]))
    ]

addCoreClasses :: EnvTransformer
addCoreClasses =     addClass "Eq" []
                 <:> addClass "Ord" ["Eq"]
--                 <:> addClass "Show" []
                 <:> addClass "Enum" []
                 <:> addClass "Num" ["Eq"]
                 <:> addClass "Real" ["Num", "Ord"]
                 <:> addClass "Fractional" ["Num"]
                 <:> addClass "Integral" ["Real", "Enum"]
                 <:> addClass "Floating" ["Fractional"]

defaultClassEnv :: EnvTransformer
defaultClassEnv =     addCoreClasses
                  <:> addInst [] (IsIn "Integral" typeInt)
                  <:> addInst [] (IsIn "Floating" typeFloat)
                  <:> addInst [] (IsIn "Eq" typeBool)

-- | Right now this is a two-pass inferencer.
-- The first pass infers and solves for generic type schemes for each
-- definition. These are then inserted into the default type environment and
-- inference and solving are re-run to produce the final type schemes.
-- TODO: handle predicates.
typecheck_defns
    :: Monad m
    => [(Symbol, CoreExpr ())]
    -> TypeEnv
    -> ExceptT PsiloError m ([Scheme], [Constraint])
typecheck_defns defns te = do
    let te' = defaultTypeEnv <> te

    -- annotate expressions
    (syms, exprs) <- mapAndUnzipM
                     (\(sym, expr) -> return (sym, annotated expr))
                     defns

    -- pass 1: add tyvar placeholders to type env, infer
    (tys_pass_1, _, cs1) <- runInfer te' initInferState $ do
        scms <- forM exprs $ \_ -> do
            tv <- fresh Star
            return $ generalize te' $ [] :=> (TVar tv)
        inferred <- withEnv (zip syms scms) $ mapM infer exprs
        return inferred
    (frame1, pm1) <- solveConstraints cs1
    let scms1 = map (\ty -> closeOver frame1 ([] :=> ty)) tys_pass_1

    -- pass 2: extend type env with results of pass 1, do it again
    let te = foldl extendEnv te' $ zip syms scms1
    (tys_pass_2, _, cs2) <- runInfer te initInferState $ mapM infer exprs
    (frame2, pm2) <- solveConstraints cs2
    let pm = substitute frame2 pm2
    let toScheme ty =
            let ty' = substitute frame2 ty
            in  closeOver frame2 ((lookupPreds ty') pm :=> ty')
    let scms2 = fmap toScheme tys_pass_2
    return (scms2, cs2)

typecheck_defn
    :: Monad m
    => (Symbol, CoreExpr ())
    -> TypeEnv
    -> ExceptT PsiloError m (Scheme, [Constraint])
typecheck_defn defn te = do
    ((ty:_), cs) <- (typecheck_defns [defn] te)
    return (ty, cs)
