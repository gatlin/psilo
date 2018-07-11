module Lib.Types.Solve where

import           Lib.Syntax.Symbol
import           Lib.Types.Constraint
import           Lib.Types.Frame
import           Lib.Types.Kind       (HasKind, Kind (..), kind)
import           Lib.Types.Type       (Pred, Rho, Sigma, TyCon (..), TyVar (..),
                                       Type (..))

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
    { frame       :: Frame
    , constraints :: [Constraint]
    , preds       :: [Pred]
    , skolemVar   :: Int
    }

initSolveState :: SolveState
initSolveState = SolveState mempty mempty mempty 0

-- | A monad for solving constraints. The state is a 'Unifier' being
-- constructed. Execution may result in a raised 'TypeError'.
type Solve = StateT SolveState Compiler

logS :: String -> Solve ()
logS = lift . logMsg

--runSolve :: Monad m => Solve m a -> SolveState -> m (Either PsiloError a)
runSolve :: Solve a -> SolveState -> Either PsiloError a
runSolve s st = compile (evalStateT s st)

showWithKind x = "(" ++ (show x) ++ " :: " ++ (show (kind x)) ++ ")"

-- | Unification of two 'Type's
unify :: Type -> Type -> Solve Unifier
unify t1 t2                       {-| (kind t1) /= (kind t2) =
                                        throwError $ OtherTypeError $
                                        "Kind mismatch: " ++
                                        (showWithKind t1) ++ " and " ++
                                        (showWithKind t2)-}
                                  | t1 == t2 = return emptyUnifier
--unify t1@(TLiteral _) t2@(TFun _) = throwError $ UnificationFail t1 t2
--unify t1@(TFun _) t2@(TLiteral _) = throwError $ UnificationFail t1 t2
unify (TVar v) t                  = v `bind` t
unify t (TVar v)                  = v `bind` t
unify t1 t2@(TForall _ _)         = unifySkolem t2 t1
unify t1@(TForall _ _) t2         = unifySkolem t2 t1
unify t1@(TFun as) t2@(TFun bs)   = unifyMany as bs
unify t1 t2                       = throwError $ UnificationFail t1 t2

-- | Unification of a list of 'Type's.
unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
    (su1, cs1) <- unify t1 t2
    (su2, cs2) <- unifyMany (substitute su1 ts1) (substitute su1 ts2)
    return (su2 `compose` su1, nub $ cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Unification of terms which might be sigma types
unifySkolem :: Sigma -> Sigma -> Solve Unifier
unifySkolem sigma1 sigma2@(TForall _ _) = do
    logS $ "Unify skolem 1: " ++ (show sigma1) ++ " <=> " ++ (show sigma2)
    (sk_vars, rho2) <- skolemize sigma2
    logS $ "Unify skolem 1.1: " ++ (show rho2)
    unify sigma1 rho2

unifySkolem sigma1@(TForall _ _) rho2 = do
    rho1 <- instantiate sigma1
    logS $ "Unify skolem 2: " ++ (show rho1) ++ " <=> " ++ (show rho2)
    unify rho1 rho2

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
            throwError $
                OtherTypeError $
                "Frame 1: " ++ (show su1) ++ "  Frame 2: " ++ (show su2)
        Just su -> return (su, nub $ cs1 ++ cs2)

-- | Bind a 'TyVar' to a 'Type' in the 'Frame', unless the result would be an
-- infinite type.
bind :: TyVar -> Type -> Solve Unifier
--bind a t@(TLiteral v) = do
--    logS $ "[bind] " ++ (show a) ++ " => " ++ (show t)
--    return emptyUnifier
--bind a (TLiteral v) = bind a (TVar v)
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
solver :: Solve (Frame, [Pred])
solver = gets constraints >>= go . sort where
    go [] = get >>= \(SolveState f _ cc sks) -> return (f, cc)

    go ((t1 := t2):cs0) = do
        su <- gets frame
        (su1, cs1) <- unify (substitute su t1) (substitute su t2)
        modify $ \st -> st {
            frame = su1 `compose` su,
            constraints = nub $ (substitute su1 cs1) ++ (substitute su1 cs0)
            }
        solver

    go (((IsIn c _)  :~ t ):cs0) = do
        su <- gets frame
        cc <- gets preds
        let t' = substitute su t
        modify $ \st -> st {
            preds = (IsIn c t') : cc,
            constraints = cs0
            }
        solver

solveConstraints
    :: [Constraint]
    -> Int
    -> Compiler (Frame, [Pred])
solveConstraints cs skolem_start = evalStateT solver $ initSolveState {
    constraints = cs,
    skolemVar = skolem_start
    }

matchTypes
    :: Type
    -> Type
    -> Compiler ()
matchTypes t1 t2 = do
    evalStateT (match t1 t2) initSolveState
    return ()

fresh :: Kind -> Solve TyVar
fresh k = do
    c <- gets skolemVar
    modify $ \st -> st { skolemVar = c + 1 }
    return $ TyVar c k

instantiate :: Sigma -> Solve Type
instantiate (TForall vs t) = do
    vs' <- mapM (const $ fresh Star) vs >>= mapM (return . TVar)
    let frame = M.fromList $ zip vs vs'
    t' <- case t of
        (ps :=> ty) -> do
            let ps' = substitute frame ps
            return $ ps' :=> (substitute frame ty)
        _ -> return $ substitute frame $ [] :=> t
    return t'

-- |
skolemize :: Sigma -> Solve ([TyVar], Rho)
skolemize (TForall vars ty) = do -- Rule PRPOLY
    sks1 <- mapM (const $ fresh Star) vars
    let frame = M.fromList $ zip vars (fmap TVar sks1)
    (sks2, ty') <- skolemize (substitute frame ty)
    return (sks1 ++ sks2, ty')

skolemize qt@(preds :=> ty) = do -- Rule, uh, PRPRED
    (sks, ty') <- skolemize ty
    return (sks, preds :=> ty')

skolemize (TFun tys) = do -- Rule PRFUN
    let tys_len = length tys
    let (arg_tys, [res_ty]) = splitAt (tys_len - 1) tys
    (sks, res_ty') <- skolemize res_ty
    return (sks, TFun $ arg_tys ++ [ res_ty' ])

skolemize ty = do -- Rule PRMONO
    return ([], ty)
