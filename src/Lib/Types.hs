{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Types
    ( typecheck
    , TypeEnv(..)
    , extendEnv
    , envLookup
    , defaultTypeEnv
    , emptyTypeEnv
    , buildTypeEnv
    , normalize
    , tyFun
    , quantify
    , removeEmptyPreds
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

import           Control.Comonad        (extend, extract, (=>>))
import           Control.Comonad.Cofree (Cofree (..))
import           Control.Monad          (foldM, forM, forM_, guard, liftM2,
                                         mapAndUnzipM, unless, zipWithM)
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.State
import           Data.Maybe             (fromJust, fromMaybe, isJust, isNothing)
import           Data.Monoid            ((<>))

import           Data.Map               (Map)
import qualified Data.Map.Lazy          as M
import           Data.Set               (Set)
import qualified Data.Set               as S

import           Data.Graph             (Graph, Vertex)
import qualified Data.Graph             as G
import qualified Data.Text              as T
import           Data.Tree              (flatten)

import           Data.List              (intercalate, sort, sortBy)
import           Data.Ord               (Ordering (..))

import           Lib.Syntax             (AnnotatedExpr, CoreAst (..), CoreExpr,
                                         SurfaceAst (..), SurfaceExpr, Symbol,
                                         TopLevel (..), annotated)

import           Lib.Types.Class
import           Lib.Types.Constraint
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Scheme
import           Lib.Types.Type
import           Lib.Types.TypeCheck
import           Lib.Types.TypeEnv

import           Lib.Compiler
import           Lib.Errors
import           Lib.Parser             (parse_expr, parse_multi)

-- * Defaults

num_binop :: Type
num_binop = [IsIn "Num" t_0] :=> (TList [tyFun, t_0, t_0, t_0])
    where t_0 = TVar (TyVar 0 Star)

integral_binop :: Type
integral_binop = [IsIn "Integral" t_0] :=> (TList [tyFun, t_0, t_0, t_0])
    where t_0 = TVar (TyVar 0 Star)

eq_binop :: Type
eq_binop = [IsIn "Eq" t_0] :=> (TList [tyFun, t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

ord_binop :: Type
ord_binop = [IsIn "Ord" t_0] :=> (TList [tyFun, t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

-- | Builtin operators and functions with explicit type schemes
defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv $ M.fromList
    [ ("*", generalize mempty num_binop)
    , ("+", generalize mempty num_binop)
    , ("-", generalize mempty num_binop)
    , ("/", generalize mempty num_binop)
    , ("modulo", generalize mempty integral_binop)
    , ("=?", generalize mempty eq_binop)
    , ("<", generalize mempty ord_binop)
    , (">", generalize mempty ord_binop)
--    , ("id", generalize mempty $ [] :=> (TList [tyFun, TVar (TyVar 0 Star),
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
                  <:> addInst [] (IsIn "Fractional" typeFloat)
                  <:> addInst [] (IsIn "Eq" typeBool)
                  <:> addInst [] (IsIn "Real" typeFloat)
                  <:> addInst [] (IsIn "Real" typeInt)
                  <:> addInst [] (IsIn "Num" typeFloat)
                  <:> addInst [] (IsIn "Num" typeInt)
                  <:> addInst [] (IsIn "Ord" typeFloat)
                  <:> addInst [] (IsIn "Ord" typeInt)
                  <:> addInst [] (IsIn "Eq" typeFloat)
                  <:> addInst [] (IsIn "Eq" typeInt)

-- | We build a dependency graph of different definitions and topologically sort
-- them. Then typechecking, as crude as it may be, is simply folding the initial
-- type environment with the 'typecheck_pass' function over the sorted list.
typecheck
    :: [(Symbol, AnnotatedExpr ())]
    -> TypeEnv
    -> Compiler TypeEnv
typecheck defns _te = do
    let te = defaultTypeEnv <> _te
    logMsg "Initial type environment"
    logMsg . show $ te
    logMsg "-----"
    classEnv <- transformCE defaultClassEnv mempty
    let dependency_graph = make_dep_graph defns
    let defns' = reverse $ topo' dependency_graph
    te' <- foldM (typecheck_pass classEnv) te defns'
    logMsg "Final type environment"
    logMsg . show $ te'
    logMsg "-----"
    return te'

typecheck_pass
    :: ClassEnv
    -> TypeEnv
    -> (Symbol, AnnotatedExpr ())
    -> Compiler TypeEnv
typecheck_pass ce te (sym, expr) = runTypeCheck te $ do
    expr' <- sequence . extend infer $ expr
    (frame, preds) <- solver `catchError` (handleTypecheckError sym)
    let sig = fmap (substitute frame) expr'
    let vars = ftv $ extract sig
    preds' <- forM preds $ \pred@(IsIn c t) -> case t of
        (TVar v) -> if S.member v vars
                    then return [substitute frame pred]
                    else return []
        _ -> return []
    let scheme = extract $ (extend $ toSigma frame (concat preds')) sig

    --return $ extendEnv te (sym, scheme)
    te' <- checkTypeEnv sym scheme te
    return te'

handleTypecheckError sym err = throwError $
    OtherError $ "For " ++ sym ++ ", " ++ (show err)

checkTypeEnv :: Symbol -> Sigma -> TypeEnv -> TypeCheck TypeEnv
checkTypeEnv sym t1 tyEnv = case envLookup tyEnv sym of
    Nothing -> return $ extendEnv tyEnv (sym, t1)
    Just t2 -> do
        let t1' = removeEmptyPreds t1
            t2' = removeEmptyPreds t2
        (frame, _) <- unify t1' t2' `catchError` (addContext sym t2)
        logTypeCheck $ "Frame: " ++ (show frame)
        return tyEnv

    where addContext sym ty err =
              throwError $ OtherError $
              "For " ++ sym ++ " : " ++ (show ty) ++ ",\n " ++ (show err)

toSigma :: Frame -> [Pred] -> AnnotatedExpr Type -> Sigma
toSigma frame preds expr =
    let ty = extract expr
    in  closeOver frame (preds :=> ty)

-- | Compute dependencies for a given expression and return as a list of Symbols
deps :: [(Symbol, AnnotatedExpr ())] -> AnnotatedExpr () -> [Symbol]
deps xs expr = go expr where
    go (() :< (IdC sym)) = case lookup sym xs of
        Nothing -> []
        Just _  -> [sym]

    go (() :< (AppC op erands)) = (go op) ++ (concatMap go erands)
    go (() :< (FunC _ body)) = go body
    go _ = []

-- oh what do you know I\'m stealing more from Stephen Diehl:
-- http://dev.stephendiehl.com/hask/#graphs

data Grph node key = Grph
  { _graph    :: Graph
  , _vertices :: Vertex -> (node, key, [key])
  }

type DepGraph = Grph (Symbol, AnnotatedExpr ()) Symbol

fromList :: Ord key => [(node, key, [key])] -> Grph node key
fromList = uncurry Grph . G.graphFromEdges'

vertexLabels :: Functor f => Grph b t -> (f Vertex) -> f b
vertexLabels g = fmap (vertexLabel g)

vertexLabel :: Grph b t -> Vertex -> b
vertexLabel g = (\(vi, _, _) -> vi) . (_vertices g)

-- Topologically sort a graph
topo' :: Grph node key -> [node]
topo' g = vertexLabels g $ G.topSort (_graph g)

-- | Traverse an expression tree and create a dependency graph
make_dep_graph defns = fromList $ fmap dep_list defns where
    dep_list (sym, expr) = ((sym, expr), sym, extract ( extend (deps defns) expr))
