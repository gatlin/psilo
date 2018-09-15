{-# LANGUAGE FlexibleContexts #-}
module Lib.Types.TypeCheck where

import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol

import           Lib.Types.Class
import           Lib.Types.Constraint
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Scheme
import           Lib.Types.Type
import           Lib.Types.TypeEnv

import           Lib.Compiler
import           Lib.Errors

import           Data.List              (nub, sort, splitAt)
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
    , predicates  :: Map (Set TyVar) [Pred]
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
t1 @= t2 = do
--    logTypeCheck $ "Constraint: " ++ (show $ t1 := t2)
    modify $ \st -> st {
        constraints = [t1 := t2] ++ (constraints st)
    }

(@~) :: Symbol -> [Type] -> TypeCheck ()
cls @~ tys = do
    modify $ \st -> st {
        constraints = [cls :~ tys] ++ (constraints st)
        }

-- | helper to record a predicate constraint

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
    return $ substitute frame t
instantiate qt = return qt

-- | Constraint generation and type generation
infer :: AnnotatedExpr (Maybe Type) -> TypeCheck Rho

-- constants are straightforward, except integers could be any @Num@ instance
infer (_ :< IntC _) = do
    ty <- fresh Star >>= \tv -> return $ TVar tv
-- TODO replace this with something
    "Num" @~ [ty]
    return ty
--    return typeInt

infer (_ :< BoolC _) = return typeBool
infer (_ :< FloatC _) = return typeFloat

infer (_ :< IdC sym) = do
    te <- getEnv
    case envLookup te sym of
        Nothing -> fresh Star >>= return . TVar
        Just scheme -> do
            ty <- instantiate scheme
            return ty

-- | A lambda abstraction is a list of symbols and an 'AnnotatedExpr' body. Each
-- argument should generate a unique 'Sigma' and the 'TypeEnv' should be
-- temporarily extended with them to evaluate the body.
infer (_ :< FunC args body) = do
    (argVars, argScms) <- mapAndUnzipM (\arg -> do
        var <- fresh Star
        return (TVar var, TForall [] (TVar var))) args
    br <- withEnv (zip args argScms) $ infer body
    let fun_ty = TList $ tyFun : (argVars ++ [br])
    return fun_ty

-- | Lambda application is straightforward: infer types for the operator and the
-- operands, generate a fresh type variable for the return value, and assert
-- athat the operator is equivalent to a function consuming the operands and
-- producing the return value.
infer (_ :< AppC op erands) = do
    op'<- do
        op'' <- infer op
        case op'' of
            ps :=> ty -> do
                forM_ ps $ \(TPred sym tys) -> sym @~ tys
                return ty
            _ -> return op''
    erands' <- mapM infer erands
    var <- fresh Star >>= return . TVar
    op'@= (TList $ tyFun : (erands' ++ [var]))
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

-- vestige
logS = logTypeCheck

emptyUnifier :: Unifier
emptyUnifier = (mempty, [])

-- | Makes the solver available elsewhere in the compiler.
runSolve :: TypeCheck a -> TypeCheckState -> Either PsiloError a
runSolve s st = compile $ do
    ((a, _)) <- runStateT (runReaderT s mempty) st
    return a

showWithKind x = "(" ++ (show x) ++ " :: " ++ (show (kind x)) ++ ")"

-- | Unification of two 'Type's
unify :: Tau -> Tau -> TypeCheck Unifier

unify (TVar tv) ty = bind tv ty
unify ty (TVar tv) = bind tv ty

unify t1@(TList as) t2@(TList bs)   = unifyMany as bs
unify t1@(TSym s1) t2@(TSym s2)
    | s1 == s2 = return emptyUnifier
    | otherwise = throwError $ UnificationFail t1 t2

-- this is almost certainly wrong
unify (TForall _ t1) t2 = unify t1 t2
unify t1 (TForall _ t2) = unify t1 t2

unify ty1@(ps1 :=> t1) ty2@(ps2 :=> t2) = do
--    logS $ "ty1 = " ++ (show ty1) ++ ", ty2 = " ++ (show ty2)
    (su, cs) <- unify t1 t2
    (su', cs') <- unifyMany (substitute su ps1) (substitute su ps2)
    return (su' `compose` su, nub $ cs ++ cs')

unify ty1@(ps :=> t1) t2 = do
--    logS $ "ps = " ++ (show ps)
    unify t1 t2

unify (TPred s1 ts1) (TPred s2 ts2)
    | s1 /= s2 = throwError $ UnificationFail (TPred s1 ts1) (TPred s2 ts2)
    | otherwise = unifyMany ts1 ts2

unify t1 t2                       = throwError $ UnificationFail t1 t2

-- | Unification of a list of 'Type's.
unifyMany :: [Type] -> [Type] -> TypeCheck Unifier
unifyMany [] [] = return emptyUnifier
unifyMany ty1@(t1 : ts1) ty2@(t2 : ts2)
    | length ty1 > length ty2 = (unifyUneven ty1 ty2) `catchError` ce
    | length ty1 < length ty2 = unifyUneven ty2 ty1 `catchError` ce
    | otherwise = do
          (su1, cs1) <- unify t1 t2
          (su2, cs2) <- unifyMany (substitute su1 ts1) (substitute su1 ts2)
          return (su2 `compose` su1, nub $ cs1 ++ cs2)

    where ce err = throwError $ OtherError $
                   "While unifying uneven type constructors " ++
                   "(" ++ (show ty1) ++ ") and (" ++ (show ty2) ++ "), " ++
                   (show err)

unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Partially applied types can throw a curveball.
unifyUneven :: [Type] -> [Type] -> TypeCheck Unifier
unifyUneven longer shorter = do
    let diff = (length longer) - (length shorter)
    let (hd, tl) = splitAt (diff+1) longer
    unifyMany ((TList hd): tl) shorter

-- | Determine if two types match. Similar to unification.
match :: Type -> Type -> TypeCheck Unifier
match t1 t2                         | t1 == t2 = return emptyUnifier
--match (TVar v) t            | (kind v) == (kind t) = return (v |-> t, [])
match (TVar v) t                    = return (v |-> t, [])
match (TList as) (TList bs)           = matchMany as bs
match t1@(TForall _ _) t2@(TForall _ _) = do
    (_, t1') <- skolemize t1
    (_, t2') <- skolemize t2
    match t1' t2'
match (ps1 :=> t1) (ps2 :=> t2) = do
    (su, cs) <- match t1 t2
    (su', cs') <- matchMany (sort $ substitute su ps1) (sort $ substitute su ps2)
    case merge su su' of
        Nothing   -> throwError $ TypeMismatch (ps1 :=> t1) (ps2 :=> t2)
        Just su'' -> return (su'', nub $ cs ++ cs')
match (TPred s1 ts1) (TPred s2 ts2)
    | s1 /= s2 = throwError $ TypeMismatch (TPred s1 ts1) (TPred s2 ts2)
    | otherwise = unifyMany ts1 ts2
match t1 t2                         = throwError $ TypeMismatch t1 t2

matchMany :: [Type] -> [Type] -> TypeCheck Unifier
matchMany t1@([]) t2@(x:xs) = throwError $ OtherTypeError $
    "Cannot match " ++ (show t2) ++ " with an empty type."
matchMany [] [] = return emptyUnifier
matchMany ty1@(t1 : ts1) ty2@(t2 : ts2)
    | length ty1 > length ty2 = matchUneven ty1 ty2
    | length ty1 < length ty2 = matchUneven ty2 ty1
    | otherwise = do
          (su1, cs1) <- match t1 t2
          (su2, cs2) <- matchMany ts1 ts2
          case merge su1 su2 of
              Nothing -> throwError $ TypeMismatch (TList ty1) (TList ty2)
              Just su -> return (su, nub $ cs1 ++ cs2)

matchMany t1 t2 = throwError $ OtherTypeError $
    "Cannot match " ++ (show t1) ++ " and " ++ (show t2)

matchUneven longer shorter = do
    let diff = (length longer) - (length shorter)
    let (hd, tl) = splitAt (diff + 1) longer
    matchMany ((TList hd):tl) shorter

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
solver :: TypeCheck (Frame, Map (Set TyVar) [Pred])
solver = gets constraints >>= go . sort where
    go [] = get >>= \(TypeCheckState  vc f cc ps ) -> return (f, ps)

    -- unify these two types
    go (c@(t1 := t2):cs0) = do
        su <- gets frame
        (_, t1') <- skolemize (substitute su (removeEmptyPreds t1))
        (_, t2') <- skolemize (substitute su (removeEmptyPreds t2))
        (su1, cs1) <- unify t1' t2'
        modify $ \st -> st {
            frame = su1 `compose` su,
            constraints = nub $ (substitute su1 cs1) ++ (substitute su1 cs0)
            }
        solver

    -- construct our elaborate stupid predicate map
    go (c:cs0) = do
        su <- gets frame
        preds <- gets predicates
        let (sym :~ tys) = substitute su c
            vars = ftv tys
            preds' = case M.lookup vars preds of
                         Nothing -> M.insert vars [TPred sym tys] preds
                         Just ps -> M.insert vars
                                    ((TPred sym tys):(substitute su ps))
                                    preds
        modify $ \st -> st {
            constraints = cs0,
            predicates = preds'
            }
        solver

-- |
skolemize :: Sigma -> TypeCheck ([TyVar], Rho)
skolemize (TForall vars ty) = do -- Rule PRPOLY
    sks1 <- mapM (const $ fresh Star) vars
    let frame = M.fromList $ zip vars (fmap TVar sks1)
    (sks2, ty') <- skolemize (substitute frame ty)
    return (sks1 ++ sks2, ty')

skolemize (TList ((TSym (TyLit "->" _)):tys)) = do -- Rule PRFUN
    let tys_len = length tys
    let (arg_tys, [res_ty]) = splitAt (tys_len - 1) tys
    (sks, res_ty') <- skolemize res_ty
    return (sks, TList $ tyFun : (arg_tys ++ [ res_ty' ]))

skolemize ty = do -- Rule PRMONO
    return ([], ty)
