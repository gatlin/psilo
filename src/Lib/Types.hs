{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Types
    ( typecheck
    , TypeEnv(..)
    , extendEnv
    , envLookup
    , defaultTypeEnv
--    , defaultClassEnv
    , emptyTypeEnv
    , buildTypeEnv
    , normalize
    , tyFun
    , quantify
    , removeEmptyPreds
--    , Class(..)
--    , ClassEnv(..)
--    , EnvTransformer(..)
--    , (<:>)
    , showType
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

import           Data.List              (elem, intercalate, sort, sortBy)
import           Data.Ord               (Ordering (..))

import           Lib.Syntax             (AnnotatedExpr, CoreAst (..), CoreExpr,
                                         SurfaceAst (..), SurfaceExpr, Symbol,
                                         TopLevel (..), annotated)

--import           Lib.Types.Class
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

-- * Default type environment

t_0 :: Type
t_0 = TVar (TyVar 0 Star)

t_1 :: Type
t_1 = TVar (TyVar 1 Star)

tyEntail :: Type
tyEntail = TSym (TyLit "=>" Star)

clsEq :: Type
clsEq = TSym (TyLit "Eq" Star)

clsOrd :: Type
clsOrd = TSym (TyLit "Ord" Star)

clsNum :: Type
clsNum = TSym (TyLit "Num" Star)

eq_binop :: Type
eq_binop = TList [tyEntail, TList [clsEq, t_0],
                  TList [ tyFun, t_0, t_0, typeBool]]

num_binop :: Type
num_binop = [TPred "Num" [t_0]] :=> (TList [tyFun, t_0, t_0, t_0])

ord_binop :: Type
ord_binop = TList [tyEntail, TList [ clsOrd, t_0],
                   TList [ tyFun, t_0, t_0, typeBool]]
--ord_binop = [IsIn "Ord" [t_0]] :=> (TList [tyFun, t_0, t_0, typeBool])
--    where t_0 = TVar (TyVar 0 Star)

not_fn :: Type
not_fn = TList [tyFun, typeBool, typeBool]

-- Builtin binary operators for specialized, non-typeclass math functions.
int_binop :: Type
int_binop = TList [tyFun, typeInt, typeInt, typeInt]

float_binop :: Type
float_binop = TList [tyFun, typeFloat, typeFloat, typeFloat]

byte_binop :: Type
byte_binop = TList [ tyFun, typeByte, typeByte, typeByte ]

eq_fn :: Type -> Type
eq_fn t = TList [tyFun, t, t, typeBool]

fromint_fn :: Type -> Type
fromint_fn t = TList [tyFun, typeInt, t]

eval_fn :: Type
eval_fn = TList [ tyFun
                , TList [ TSym (TyLit "FFI" Star)
                        , t_0
                        ]
                , TList [ tyFun
                        , t_0
                        , t_1
                        ]
                , t_1
                ]

-- | Builtin operators and functions with explicit type schemes
defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv $ M.fromList
    [ ("=?", generalize mempty $ [TPred "Eq" [t_0]] :=> (eq_fn t_0))
    , ("not", generalize mempty not_fn)
    , ("boolean-fromint", generalize mempty (fromint_fn typeBool))
    , ("boolean=?", generalize mempty (eq_fn typeBool))
    , ("int-add", generalize mempty int_binop)
    , ("int-sub", generalize mempty int_binop)
    , ("int-mul", generalize mempty int_binop)
    , ("int-div", generalize mempty int_binop)
    , ("int-modulo", generalize mempty int_binop)
    , ("int-fromint", generalize mempty (fromint_fn typeInt))
    , ("int=?", generalize mempty (eq_fn typeInt))
    , ("float-add", generalize mempty float_binop)
    , ("float-sub", generalize mempty float_binop)
    , ("float-mul", generalize mempty float_binop)
    , ("float-div", generalize mempty float_binop)
    , ("float-modulo", generalize mempty float_binop)
    , ("float=?", generalize mempty (eq_fn typeFloat))
    , ("float-fromint", generalize mempty (fromint_fn typeFloat))
    , ("byte-and", generalize mempty byte_binop)
    , ("byte-or", generalize mempty byte_binop)
    , ("byte-not", generalize mempty byte_binop)
    , ("byte-xor", generalize mempty byte_binop)
    , ("byte-2c", generalize mempty byte_binop)
    , ("byte=?", generalize mempty (eq_fn typeByte))
    , ("eval", generalize mempty eval_fn)
    , ("+", generalize mempty num_binop)
    , ("*", generalize mempty num_binop)
    , ("-", generalize mempty num_binop)
    , ("/", generalize mempty num_binop)
    , ("modulo", generalize mempty num_binop)
    --("=?", generalize mempty eq_binop)
    --, ("<", generalize mempty ord_binop)
    --, (">", generalize mempty ord_binop)
    ]

-- | We build a dependency graph of different definitions and topologically sort
-- them. Then typechecking, as crude as it may be, is simply folding the initial
-- type environment with the 'typecheck_pass' function over the sorted list.
typecheck :: TopLevel-> Compiler TopLevel
typecheck (TopLevel defs sigs tydefs) = do
    let te = defaultTypeEnv <> (TypeEnv sigs)
    let dependency_graph = make_dep_graph defs
    let defs' = reverse $ topo' dependency_graph
    (TypeEnv sigs', defs'') <- foldM typecheck_pass (te, defs) defs'
    return $ TopLevel defs'' sigs' tydefs

typecheck_pass
    :: (TypeEnv, Map Symbol (AnnotatedExpr (Maybe Type)))
    -> (Symbol, AnnotatedExpr (Maybe Type))
    -> Compiler (TypeEnv, Map Symbol (AnnotatedExpr (Maybe Type)))
typecheck_pass (te, defns) (sym, expr) = runTypeCheck te $ do
    expr' <- sequence . extend infer $ expr
    (frame, preds) <- solver `catchError` (handleTypecheckError sym te)
    let sig = fmap (substitute frame) expr' :: AnnotatedExpr Type
    let vars = ftv $ extract sig
    let ps = predicatesForSignature preds vars frame
    let scheme = extract $ (extend $ toSigma frame ps) sig
    te' <- checkTypeEnv sym scheme te
    return (te', M.insert sym (fmap Just sig) defns)

-- | Stupid and slow but necessary method to support MPTCs.
-- Iterate through every
predicatesForSignature :: Map (Set TyVar) [Pred] -> Set TyVar -> Frame -> [Pred]
predicatesForSignature predMap tvs su = M.foldMapWithKey go predMap
    where go predTvs ps = if predTvs `S.isSubsetOf` tvs
                          then (substitute su ps)
                          else []

handleTypecheckError sym te err = case envLookup te sym of
    Nothing -> throwError $ OtherError $ "For " ++ sym ++ ", " ++ (show err)
    Just ty -> throwError $ OtherError $
        "For " ++ sym ++ " : " ++ (show ty) ++ ", " ++ (show err)

checkTypeEnv :: Symbol -> Sigma -> TypeEnv -> TypeCheck TypeEnv
checkTypeEnv sym t1 tyEnv = case envLookup tyEnv sym of
    Nothing -> return $ extendEnv tyEnv (sym, t1)
    Just t2 -> do
        let t1' = removeEmptyPreds t1
        let t2' = removeEmptyPreds t2
        (_, s1) <- skolemize t1'
        (_, s2) <- skolemize t2'
        (frame, _) <- match s1 s2 `catchError` (addContext sym t2)
        return tyEnv

    where addContext sym ty err =
              throwError $ OtherError $
              "For " ++ sym ++ " : " ++ (show ty) ++ ",\n " ++ (show err)

toSigma :: Frame -> [Pred] -> AnnotatedExpr Type -> Sigma
toSigma frame ps expr =
    let ty = extract expr
    in  closeOver frame (ps :=> ty)

-- | Compute dependencies for a given expression and return as a list of Symbols
--deps :: [(Symbol, AnnotatedExpr ())] -> AnnotatedExpr () -> [Symbol]
deps :: Map Symbol (AnnotatedExpr (Maybe Type)) -> AnnotatedExpr (Maybe Type) -> [Symbol]
deps xs expr = go expr where
    go (_ :< (IdC sym)) = case M.lookup sym xs of
        Nothing -> []
        Just _  -> [sym]

    go (_ :< (AppC op erands)) = (go op) ++ (concatMap go erands)
    go (_ :< (FunC _ body)) = go body
    go _ = []

-- oh what do you know I\'m stealing more from Stephen Diehl:
-- http://dev.stephendiehl.com/hask/#graphs

data Grph node key = Grph
  { _graph    :: Graph
  , _vertices :: Vertex -> (node, key, [key])
  }

type DepGraph = Grph (Symbol, AnnotatedExpr (Maybe Type)) Symbol

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
make_dep_graph defns = fromList $ M.elems $ M.mapWithKey dep_list defns where
    dep_list sym expr = ((sym, expr), sym, extract ( extend (deps defns) expr))
