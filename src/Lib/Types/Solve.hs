module Lib.Types.Solve where

import           Lib.Syntax.Symbol
import           Lib.Types.Constraint
import           Lib.Types.Frame
import           Lib.Types.Kind       (HasKind, Kind (..), kind)
import           Lib.Types.Type       (Pred, Rho, Sigma, TyCon (..), TyVar (..),
                                       Type (..), removeEmptyPreds)

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Either          (either)
import           Data.List            (nub, sort)

import           Lib.Compiler
import           Lib.Errors

import           Lib.Types.TypeCheck

type Unifier = (Frame, [Constraint])

logS = logTypeCheck

emptyUnifier :: Unifier
emptyUnifier = (mempty, [])

--runSolve :: Monad m => Solve m a -> SolveState -> m (Either PsiloError a)
runSolve :: TypeCheck a -> TypeCheckState -> Either PsiloError a
runSolve s st = compile $ do
    ((a, _), cs) <- runWriterT $ runStateT (runReaderT s mempty) st
    return a

showWithKind x = "(" ++ (show x) ++ " :: " ++ (show (kind x)) ++ ")"

-- | Unification of two 'Type's
unify :: Type -> Type -> TypeCheck Unifier
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
unify t1 t2@(TForall _ _)         = do
    (_, t2') <- skolemize t2
    unify t1 t2'
unify t1@(TForall _ _) t2         = do
    (_, t1') <- skolemize t1
    unify t1' t2
unify t1@(TFun as) t2@(TFun bs)   = unifyMany as bs
unify t1 t2                       = throwError $ UnificationFail t1 t2

-- | Unification of a list of 'Type's.
unifyMany :: [Type] -> [Type] -> TypeCheck Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
    (su1, cs1) <- unify t1 t2
    (su2, cs2) <- unifyMany (substitute su1 ts1) (substitute su1 ts2)
    return (su2 `compose` su1, nub $ cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Determine if two types match. Similar to unification.
match :: Type -> Type -> TypeCheck Unifier
match t1 t2                 | t1 == t2 = return emptyUnifier
match (TVar v) t            | (kind v) == (kind t) = return (v |-> t, [])
match (_ :=> t1) (_ :=> t2) = match t1 t2
match (TFun as) (TFun bs)   = matchMany as bs
match (TForall _ t1) (TForall _ t2) = do
    (_, rho1) <- skolemize t1
    (_, rho2) <- skolemize t2
    match t1 t2
match t1 t2                 = throwError $ TypeMismatch t1 t2

matchMany :: [Type] -> [Type] -> TypeCheck Unifier
matchMany [] (x:xs) = return emptyUnifier
matchMany _ [] = return emptyUnifier
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
bind :: TyVar -> Type -> TypeCheck Unifier
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
solver :: TypeCheck (Frame, [Pred])
solver = gets constraints >>= go . sort where
    go [] = get >>= \(TypeCheckState  vc f cc ps) -> return (f, ps)

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

matchTypes
    :: Type
    -> Type
    -> TypeCheck ()
matchTypes t1 t2 = do
    (_, s1) <- skolemize t1
    (_, s2) <- skolemize t2
    (match (removeEmptyPreds s1) (removeEmptyPreds s2))
    return ()

-- |
skolemize :: Sigma -> TypeCheck ([TyVar], Rho)
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
