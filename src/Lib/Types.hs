{-# LANGUAGE OverloadedStrings #-}

module Lib.Types
    ( typecheck_defn
    , typecheck_defns
    , TypeEnv(..)
    , extendEnv
    , envLookup
    , defaultTypeEnv
    , emptyTypeEnv
    , Scheme(..)
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
import Control.Comonad (extend, extract, (=>>))
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
                 <:> addClass "Show" []
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

typecheck_defns
    :: [(Symbol, CoreExpr ())]
    -> TypeEnv
    -> Except PsiloError ([(Symbol, AnnotatedExpr Scheme)])
typecheck_defns defns te = do
    let te' = defaultTypeEnv <> te
    (syms, exprs) <- mapAndUnzipM (\(s,e) -> return (s, annotated e)) defns
    (inferred, _, cs) <- runInfer te' initInferState $
        mapM (sequence . extend infer) exprs
    (frame, pm) <- solveConstraints cs
    let schemes = fmap (extend $ toScheme frame pm) inferred
    return $ zip syms schemes

typecheck_defn
    :: (Symbol, CoreExpr ())
    -> TypeEnv
    -> Except PsiloError (Symbol, AnnotatedExpr Scheme)
typecheck_defn defn te = do
    ((e, s):_) <- (typecheck_defns [defn] te)
    return (e, s)

toScheme frame pm expr  =
    let ty = extract expr
        ty' = substitute frame ty
    in  closeOver frame ((lookupPreds ty') pm :=> ty')
