module Lib.Types.Solve where

import Lib.Syntax.Symbol
import Lib.Types.Kind (Kind(..), HasKind)
import Lib.Types.Type (TyVar(..), TyCon(..), Type(..))
import Lib.Types.Qual
import Lib.Types.Frame
import Lib.Types.PredMap
import Lib.Types.Constraint
import Lib.Types.TypeError

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import Data.Either (either)
import Data.List (nub)

type Unifier = (Frame, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (mempty, [])

data SolveState = SolveState
    { frame :: Frame
    , constraints :: [Constraint]
    , predMap :: PredMap
    }

initSolveState :: SolveState
initSolveState = SolveState mempty mempty mempty

-- | A monad for solving constraints. The state is a 'Unifier' being
-- constructed. Execution may result in a raised 'TypeError'.
type Solve = StateT SolveState (Except TypeError)

runSolve :: Solve a -> SolveState -> Either TypeError a
runSolve s st = runExcept (evalStateT s st)

-- | Unification of two 'Type's
unify :: Type -> Type -> Solve Unifier
unify t1 t2 | t1 == t2 = return emptyUnifier
unify (TVar v) t = v `bind` t
unify t (TVar v) = v `bind` t
unify (TFun as) (TFun bs) = unifyMany as bs
unify t1 t2 = throwError $ UnificationFail t1 t2

-- | Unification of a list of 'Type's.
unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
    (su1, cs1) <- unify t1 t2
    (su2, cs2) <- unifyMany (substitute su1 ts1) (substitute su1 ts2)
    return (su2 `compose` su1, nub $ cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Bind a 'TyVar' to a 'Type' in the 'Frame', unless the result would be an
-- infinite type.
bind :: TyVar -> Type -> Solve Unifier
bind a t | t == TVar a = return emptyUnifier
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = do
               return (M.singleton a t, [])

-- | Ensure that a 'TyVar' is not free in a 'Type'.
occursCheck :: TypeLike a => TyVar -> a -> Bool
occursCheck a t = a `S.member` (ftv t)

-- | The actual solving algorithm. Reads the current 'Unifier' and iterates
-- through the constraints generating 'Unifier's. These are then merged into the
-- state.
solver :: Solve (Frame, PredMap)
solver = do
    cs <- gets constraints
    case cs of
        [] -> get >>= \(SolveState f _ p) -> return (f, p)
        (c:cs0) -> case c of
            (t1 := t2) -> do
                su <- gets frame
                (su1, cs1) <- unify t1 t2
                modify $ \st -> st {
                    frame = su1 `compose` su,
                    constraints = nub $ cs1 ++ (substitute su1 cs0) }
                solver
            (ty :~ ps) -> do
                su <- gets frame
                pm <- gets predMap
                let pm' = updatePredMap ty ps pm
                put $ SolveState su cs0 $ substitute su pm'
                solver

solveConstraints :: [Constraint] -> Except TypeError (Frame, PredMap)
solveConstraints cs = evalStateT solver $ initSolveState {
    constraints = cs
    }

-- * Defaults!