{-# LANGUAGE OverloadedStrings #-}

module Lib.Types
    ( typecheck
    , TypeEnv(..)
    , extendEnv
    , envLookup
    , defaultTypeEnv
    , emptyTypeEnv
    , buildTypeEnv
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

import Control.Monad
    ( forM_
    , forM
    , foldM
    , liftM2
    , zipWithM
    , mapAndUnzipM
    , guard
    )
import Control.Monad.Except
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

import Lib.Compiler (Compiler, Log, logMsg)
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
--    , ("+", generalize mempty num_binop)
--    , ("-", generalize mempty num_binop)
--    , ("/", generalize mempty num_binop)
--    , ("=", generalize mempty eq_binop)
--    , ("<", generalize mempty ord_binop)
--    , (">", generalize mempty ord_binop)
--    , ("id", generalize mempty $ [] :=> (TFun [TVar (TyVar 0 Star),
--                                               TVar (TyVar 0 Star)]))
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

-- | This is a two-pass situation.
typecheck
    :: [(Symbol, CoreExpr ())]
    -> TypeEnv
    -> Compiler ([(Symbol, AnnotatedExpr Scheme)], TypeEnv)
typecheck defns _te = do
    let te = defaultTypeEnv <> _te
    logMsg $ "te: " ++ (show te)
    (syms, exprs) <- mapAndUnzipM (\(s,e) -> return (s, annotated e)) defns
    exprs' <- sequence exprs
    (schemes, te') <- typecheck_pass (syms, exprs') te
    logMsg $ "te' [2]: " ++ (show te')
    return (zip syms schemes, te')
--    (_, te') <- typecheck_pass (syms, exprs') te
--    (schemes, te'') <- typecheck_pass (syms, exprs') te'
--    return (zip syms schemes, te'')

typecheck_pass
    :: ([Symbol], [AnnotatedExpr ()])
    -> TypeEnv
    -> Compiler ([AnnotatedExpr Scheme], TypeEnv)
typecheck_pass (syms, exprs) te = do
    (exprs', inferState, cs) <- runInfer te $
        mapM (sequence . extend infer) exprs
    logMsg "Expressions"
    forM_ (zip syms exprs') $ logMsg . show
    (frame, pm) <- solveConstraints cs
    logMsg "Constraints"
    forM_ cs $ logMsg . show
    let schemes = fmap (extend $ toScheme frame pm) exprs'
    let te' = buildTypeEnv $ zip syms (fmap extract schemes)
    logMsg $ "te' [1]: " ++ (show te')
    logMsg $ "(typeEnv inferState): " ++ (show $ typeEnv inferState)
    return $ (schemes, substitute frame $  (typeEnv inferState) <> te')

toScheme :: Frame -> PredMap -> AnnotatedExpr Type -> Scheme
toScheme frame pm expr =
    let ty = extract expr
        ty' = substitute frame ty
        pm' = substitute frame pm
    in  closeOver frame ((lookupPreds ty') pm' :=> ty')
