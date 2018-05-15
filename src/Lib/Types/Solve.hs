module Lib.Types.Solve where

import           Lib.Syntax.Symbol
import           Lib.Types.Constraint
import           Lib.Types.Frame
import           Lib.Types.Kind       (HasKind, Kind (..), kind)
import           Lib.Types.Type       (Pred, TyCon (..), TyVar (..), Type (..))

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Either          (either)
import           Data.List            (nub, sort)

import           Lib.Compiler
import           Lib.Errors

type Unifier = (Frame, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (mempty, [])

data SolveState = SolveState
    { frame            :: Frame
    , constraints      :: [Constraint]
    , classConstraints :: [Constraint]
    }

initSolveState :: SolveState
initSolveState = SolveState mempty mempty mempty

-- | A monad for solving constraints. The state is a 'Unifier' being
-- constructed. Execution may result in a raised 'TypeError'.
type Solve = StateT SolveState Compiler

logS :: String -> Solve ()
logS = lift . logMsg

--runSolve :: Monad m => Solve m a -> SolveState -> m (Either PsiloError a)
runSolve :: Solve a -> SolveState -> Either PsiloError a
runSolve s st = compile (evalStateT s st)

-- | Unification of two 'Type's
unify :: Type -> Type -> Solve Unifier
unify t1 t2                     | t1 == t2 = return emptyUnifier
unify (TVar v) t                = v `bind` t
unify t (TVar v)                = v `bind` t
unify t1@(TFun as) t2@(TFun bs) = unifyMany as bs
unify t1 t2                     = throwError $ UnificationFail t1 t2

-- | Unification of a list of 'Type's.
unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
    (su1, cs1) <- unify t1 t2
    (su2, cs2) <- unifyMany (substitute su1 ts1) (substitute su1 ts2)
    return (su2 `compose` su1, nub $ cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Determine if two types match. Similar to unification.
match :: Type -> Type -> Solve Unifier
match t1 t2               | t1 == t2 = return emptyUnifier
match (TVar v) t          | (kind v) == (kind t) = return (v |-> t, [])
match (TFun as) (TFun bs) = matchMany as bs
match t1 t2               = throwError $ OtherTypeError "Error matching types"

matchMany :: [Type] -> [Type] -> Solve Unifier
matchMany [] [] = return emptyUnifier
matchMany (t1 : ts1) (t2 : ts2) = do
    (su1, cs1) <- match t1 t2
    (su2, cs2) <- matchMany ts1 ts2
    case merge su1 su2 of
        Nothing -> do
            throwError $ OtherTypeError $ "Frame 1: " ++ (show su1) ++ "  Frame 2: " ++ (show su2)
        Just su -> return (su, nub $ cs1 ++ cs2)

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
solver :: Solve (Frame, [Constraint])
solver = do
    cs <- gets constraints
    case (sort cs) of
        [] -> get >>= \(SolveState f _ cc) -> return (f, cc)
        ((t1 := t2):cs0) -> do
            su <- gets frame
            (su1, cs1) <- unify (substitute su t1) (substitute su t2)
            modify $ \st -> st {
                frame = su1 `compose` su,
                constraints = nub $ (substitute su1 cs1) ++ (substitute su1 cs0)
                }
            solver

        ((p  :~ t ):cs0) -> do
            su <- gets frame
            cc <- gets classConstraints
            let p' = substitute su p
                t' = substitute su t
            modify $ \st -> st {
                classConstraints = (p' :~ t') : cc,
                constraints = cs0
                }
            solver

solveConstraints
    :: [Constraint]
    -> Compiler (Frame, [Constraint])
solveConstraints cs = evalStateT solver $ initSolveState {
    constraints = cs
    }

matchTypes
    :: Type
    -> Type
    -> Compiler ()
matchTypes t1 t2 = do
    evalStateT (match t1 t2) initSolveState
    return ()
