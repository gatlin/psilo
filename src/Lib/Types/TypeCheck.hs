module Lib.Types.TypeCheck where

import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol

import           Lib.Types.Constraint
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Scheme
import           Lib.Types.Type
import           Lib.Types.TypeEnv

import           Lib.Compiler
import           Lib.Errors

import           Data.List              (nub, sort)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           Data.Set               (Set)
import qualified Data.Set               as S

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity

import           Control.Comonad
import           Control.Comonad.Cofree

-- * The TypeCheck monad is where inference and solving of type constraints
-- happens.

-- | The state for the type checker. This contains a monotonically increasing
-- integer to form new type variables; a substitution frame (used mostly by the
-- solver); a list of 'Constraint's generated during inference to be solved
-- later; and a list of type predicates.
data TypeCheckState = TypeCheckState
    { varCount    :: Int
    , frame       :: Frame
    , constraints :: [Constraint]
    , preds       :: [Pred]
    } deriving (Eq, Show)

initTypeCheckState :: TypeCheckState
initTypeCheckState = TypeCheckState 0 mempty mempty mempty

-- | The TypeCheck monad consists of a read-only 'TypeEnv' and a mutable
-- 'TypeCheckState'.
type TypeCheck = ReaderT TypeEnv (StateT TypeCheckState Compiler)

-- I know it isn't pretty
logTypeCheck :: String -> TypeCheck ()
logTypeCheck = lift . lift . logMsg

fresh :: Kind -> TypeCheck TyVar
fresh k = do
    c <- gets varCount
    modify $ \st -> st { varCount = c + 1 }
    return $ TyVar c k

-- * Type Inference

-- | Perform a type check operation and return it to the 'Compiler'.
runTypeCheck
    :: TypeEnv
    -> TypeCheck a
    -> Compiler (a)
runTypeCheck te m = runTypeCheck' te initTypeCheckState m

runTypeCheck'
    :: TypeEnv
    -> TypeCheckState
    -> TypeCheck a
    -> Compiler (a)
runTypeCheck' te tcState m = do
    (a, tcState') <- runStateT (runReaderT m te) tcState
    return a

-- | helper to record an equality constraint
(@=) :: Type -> Type -> TypeCheck ()
t1 @= t2 = modify $ \st -> st {
    constraints = [t1 := t2] ++ (constraints st)
    }

-- | helper to record a predicate constraint
(@~) :: Pred -> Type -> TypeCheck ()
p @~ t = modify $ \st -> st {
    constraints = [p :~ t] ++ (constraints st)
    }

tyInst :: [Pred] -> TypeCheck ()
tyInst [] = return ()
tyInst ps = forM_ ps $ \pred -> do
    let tvs = S.toList . ftv $ pred
    forM_ tvs $ \tv -> pred @~ (TVar tv)

-- | Locally modify a 'TypeEnv', do some work, and then revert it.
withEnv :: [(Symbol, Sigma)] -> TypeCheck a -> TypeCheck a
withEnv ss m = local (<> (buildTypeEnv ss)) m

getEnv :: TypeCheck TypeEnv
getEnv = ask

-- | Instantiate a 'Sigma' into a 'Rho' type
instantiate :: Sigma -> TypeCheck Rho
instantiate (TForall vs t) = do
    vs' <- mapM (const $ fresh Star) vs >>= mapM (return . TVar)
    let frame = M.fromList $ zip vs vs'
    t' <- case t of
        (ps :=> ty) -> do
            let ps' = substitute frame ps
            return $ ps' :=> (substitute frame ty)
        _ -> return $ substitute frame $ [] :=> t
    return t'
--        ps' = substitute frame ps
--    tyInst ps
--    return $ substitute frame (ps' :=> t)
instantiate qt = return qt

-- | Constraint generation and type generation
infer :: AnnotatedExpr () -> TypeCheck Rho

-- constants are straightforward, except integers could be any @Num@ instance
infer (_ :< IntC _) = do
    ty <- fresh Star >>= \tv -> return $ TVar tv
    tyInst [IsIn "Num" ty]
    return ty

infer (_ :< BoolC _) = return $ typeBool
infer (_ :< FloatC _) = return $ typeFloat

infer (_ :< IdC sym) = do
    te <- getEnv
    case envLookup te sym of
        Nothing -> fresh Star >>= return . TVar
        Just scheme -> do
            ty <- instantiate scheme
            case ty of
                (ps :=> ty') -> do
                    tyInst ps
                    return ty'
                _ -> return ty

-- | A lambda abstraction is a list of symbols and an 'AnnotatedExpr' body. Each
-- argument should generate a unique 'Sigma' and the 'TypeEnv' should be
-- temporarily extended with them to evaluate the body.
infer (_ :< FunC args body) = do
    (argVars, argScms) <- mapAndUnzipM (\arg -> do
        var <- fresh Star >>= return . TVar
        return (var, TForall [] var)) args
    br <- withEnv (zip args argScms) $ infer body
    let fun_ty = TFun $ tyFun : (argVars ++ [br])
    return fun_ty

-- | Lambda application is straightforward: infer types for the operator and the
-- operands, generate a fresh type variable for the return value, and assert
-- athat the operator is equivalent to a function consuming the operands and
-- producing the return value.
infer (_ :< AppC op erands) = do
    op' <- infer op
    erands' <- mapM infer erands
    var <- fresh Star >>= return . TVar
    op' @= (TFun $ tyFun : (erands' ++ [var]))
    return var

-- | Assert that the condition expression is a boolean and that the two branches
-- are the same thing
infer (_ :< IfC c t e) = do
    cr <- infer c
    tr <- infer t
    er <- infer e
    cr @= typeBool
    tr @= er
    return tr

-- * Solving type constraints
type Unifier = (Frame, [Constraint])

logS = logTypeCheck

emptyUnifier :: Unifier
emptyUnifier = (mempty, [])

--runSolve :: Monad m => Solve m a -> SolveState -> m (Either PsiloError a)
runSolve :: TypeCheck a -> TypeCheckState -> Either PsiloError a
runSolve s st = compile $ do
    ((a, _)) <- runStateT (runReaderT s mempty) st
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
    match t2 t1
match t1 t2                 = throwError $ TypeMismatch t1 t2

matchMany :: [Type] -> [Type] -> TypeCheck Unifier
matchMany t1@([]) t2@(x:xs) = throwError $ OtherTypeError $
    "Cannot match " ++ (show t2) ++ " with an empty type."
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
matchMany t1 t2 = throwError $ OtherTypeError $
    "Cannot match " ++ (show t1) ++ " and " ++ (show t2)

-- | Bind a 'TyVar' to a 'Type' in the 'Frame', unless the result would be an
-- infinite type.
bind :: TyVar -> Type -> TypeCheck Unifier
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
