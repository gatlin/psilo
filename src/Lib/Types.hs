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
    , unless
    )
import Control.Monad.Except
import Control.Monad.Free
import Control.Comonad (extend, extract, (=>>))
import Control.Comonad.Cofree (Cofree(..))
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid ((<>))

import Data.Map (Map)
import qualified Data.Map.Lazy as M

import Data.Graph (Graph, Vertex)
import Data.Tree (flatten)
import qualified Data.Graph as G
import qualified Data.Text as T

import Data.List (sort, sortBy)
import Data.Ord (Ordering(..))

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
import Lib.Types.Solve hiding (predMap)

import Lib.Compiler
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
    classEnv <- transformCE defaultClassEnv mempty
    logMsg $ "Class Env = " ++ (show classEnv)
    let dependency_graph = make_dep_graph defns
    let defns' = reverse $ topo' dependency_graph
    te' <- foldM (typecheck_pass classEnv) te defns'
    return te'

typecheck_pass
    :: ClassEnv
    -> TypeEnv
    -> (Symbol, AnnotatedExpr ())
    -> Compiler TypeEnv
typecheck_pass ce te (sym, expr) = do
    (sig, inferState, cs) <- runInfer te $
        sequence . extend infer $ expr
    (frame, pm) <- solveConstraints cs $ predMap inferState
    verifyPredMap pm ce
    -- build the new type environment
    let scheme = extract $ (extend $ toScheme frame pm) sig
    let te' = substitute frame $ buildTypeEnv $ [(sym, scheme)]
    return (te' <> te)

verifyPredMap :: PredMap -> ClassEnv -> Compiler ()
verifyPredMap (PMap pm) ce = forM_ (M.toList pm) $ \(ty, preds) ->
    case ty of
        (TSym _) -> forM_ preds $ \(IsIn c t) -> do
            let instances = fmap (\(_ :=> (IsIn _ x)) -> x) (insts ce c)
            unless (elem ty instances) $ throwError $
                NoClassForInstance c (show ty)
            return ()
        _ -> return ()

toScheme :: Frame -> PredMap -> AnnotatedExpr Type -> Scheme
toScheme frame pm expr =
    let ty = extract expr
        ty' = substitute frame ty
        pm' = substitute frame pm
    in  closeOver frame ((lookupPreds ty') pm' :=> ty')

deps :: [(Symbol, AnnotatedExpr ())] -> AnnotatedExpr () -> [Symbol]
deps xs expr = go expr where
    go (() :< (IdC sym)) = case lookup sym xs of
        Nothing -> []
        Just _ -> [sym]

    go (() :< (AppC op erands)) = (go op) ++ (concatMap go erands)
    go (() :< (FunC _ body)) = go body
    go _ = []

-- oh what do you know I\'m stealing more from Stephen Diehl:
-- http://dev.stephendiehl.com/hask/#graphs

data Grph node key = Grph
  { _graph :: Graph
  , _vertices :: Vertex -> (node, key, [key])
  }

type DepGraph = Grph (Symbol, AnnotatedExpr ()) Symbol

fromList :: Ord key => [(node, key, [key])] -> Grph node key
fromList = uncurry Grph . G.graphFromEdges'

vertexLabels :: Functor f => Grph b t -> (f Vertex) -> f b
vertexLabels g = fmap (vertexLabel g)

vertexLabel :: Grph b t -> Vertex -> b
vertexLabel g = (\(vi, _, _) -> vi) . (_vertices g)

-- Topologically sort graph
topo' :: Grph node key -> [node]
topo' g = vertexLabels g $ G.topSort (_graph g)

make_dep_graph defns = fromList $ fmap dep_list defns where
    dep_list (sym, expr) = ((sym, expr), sym, extract ( extend (deps defns) expr))
