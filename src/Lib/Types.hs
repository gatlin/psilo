{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types
    ( typecheck_defn
    , typecheck_defns
    , TypeEnv(..)
    , TypeError(..)
    , extendEnv
    , envLookup
    , test
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

import Control.Monad (forM_, forM, foldM, liftM2, zipWithM, mapAndUnzipM, when)
import Control.Monad.Free
import Data.Functor.Identity
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List (nub, union, intercalate)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid
import Data.Either (either)

import Control.Comonad
import Control.Comonad.Cofree

import Data.Foldable (Foldable, fold)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Lib.Syntax ( Symbol
                  , CoreExpr
                  , CoreAst(..)
                  , Definition(..)
                  , AnnotatedExpr
                  , annotated
                  , surfaceToDefinition
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
import Lib.Types.TypeError

import Lib.Parser (parse_expr, parse_multi)

num_binop :: Qual Type
num_binop = [IsIn "Num" t_0] :=> (TFun [t_0, t_0, t_0])
    where t_0 = TVar (TyVar 0 Star)

eq_binop :: Qual Type
eq_binop = [IsIn "Eq" t_0] :=> (TFun [t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

ord_binop :: Qual Type
ord_binop = [IsIn "Ord" t_0] :=> (TFun [t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

-- | The default type environment.

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
    :: [Definition]
    -> TypeEnv
    -> Either TypeError ([Scheme], [Constraint])
typecheck_defns defns te = runExcept $ do
    let te' = defaultTypeEnv <> te

    -- annotate expressions
    (syms, exprs) <- mapAndUnzipM
                     (\(Define sym expr) -> return (sym, annotated expr))
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
    :: Definition
    -> TypeEnv
    -> Either TypeError (Scheme, [Constraint])
typecheck_defn defn te = case typecheck_defns [defn] te of
    Left err -> Left err
    Right ((ty:_), cs) -> Right (ty, cs)

-- * Test shit

example_defns = [ "(def three (id 3.0))"
                , "(defun times-2 (x) (* x 2.0))"
                , "(def eight (times-2 4.0))"
                , "(defun square (x) (* x x))"
                , "(def nine (square 3.0))"
                , "(def four (square 2))"
                , "(defun fact (n) (if (< n 2) n (fact (* n (- n 1)))))"
                , "(defun compose (f g x) (f (g x)))"
                ]

test :: IO ()
test = do
    mDefns <- parse_multi . T.pack . concat $ example_defns
    case mDefns of
        Nothing -> putStrLn "Parser error T_T"
        Just defns -> do
            let defns' = fmap (fromJust . surfaceToDefinition) defns
            case typecheck_defns defns' defaultTypeEnv of
                Left err -> putStrLn . show $ err
                Right (schemes, cs) -> do
                    putStrLn "Schemes"
                    putStrLn "---"
                    forM_ (zip defns' schemes) $ \(d, s) -> do
                        let (Define sym _) = d
                        putStrLn $ sym ++ " : " ++ (show s)
{-
                    putStrLn "Constraints"
                    putStrLn "---"
                    forM_ cs $ putStrLn . show
-}
