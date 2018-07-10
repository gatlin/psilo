{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types.Infer where

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

import           Data.List              (nub)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Identity

import           Control.Comonad
import           Control.Comonad.Cofree

-- | The state we need to mutate during type inference
data InferState = InferState
    { varCount :: Int -- ^ monotonically increasing type ID number
    } deriving (Show)

initInferState :: InferState
initInferState = InferState 0

{-
newtype Infer a =
    Infer (StateT InferState (WriterT [Constraint] (Except PsiloError)) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState InferState
             , MonadWriter [Constraint]
             , MonadError PsiloError
             )
-}

type Infer = ReaderT TypeEnv (StateT InferState (WriterT [Constraint] Compiler))

runInfer
    :: TypeEnv
    -> Infer  a
    -> Compiler (a, InferState, [Constraint])

runInfer te m = do
    ((a, inferState'), cs) <- runWriterT (runStateT (runReaderT m te) initInferState)
    return (a, inferState', (nub cs))

runInfer'
    :: TypeEnv
    -> InferState
    -> Infer a
    -> Compiler (a, InferState, [Constraint])

runInfer' te inferState m = do
    ((a, inferState'), cs) <- runWriterT (runStateT (runReaderT m te) inferState)
    return (a, inferState', (nub cs))

-- I know it isn't pretty
logInfer :: String -> Infer ()
logInfer = lift . lift . lift . logMsg

-- | helper to record an equality constraint
(@=) :: Type -> Type -> Infer ()
t1 @= t2 = tell [t1 := t2]

(@~) :: Pred -> Type -> Infer ()
p @~ t = tell [ p :~ t ]

tyInst :: [Pred] -> Infer ()
tyInst [] = return ()
tyInst ps = forM_ ps $ \pred -> do
    let tvs = S.toList . ftv $ pred
    forM_ tvs $ \tv -> pred @~ (TVar tv)

withEnv :: [(Symbol, Sigma)] -> Infer a -> Infer a
withEnv ss m = local (<> (buildTypeEnv ss)) m

getEnv :: Infer TypeEnv
getEnv = ask

-- | Generate a fresh type variable with a specific 'Kind'
fresh :: Kind -> Infer TyVar
fresh k = do
    c <- gets varCount
    modify $ \st -> st { varCount = c + 1 }
    return $ TyVar c k

-- | Instantiate a type scheme into a qualified type
instantiate :: Sigma -> Infer Type
instantiate (ps :=> (TForall vs t)) = do
    vs' <- mapM (const $ fresh Star) vs >>= mapM (return . TVar)
    let frame = M.fromList $ zip vs vs'
        ps' = substitute frame ps
    tyInst ps
    return $ substitute frame (ps' :=> t)
instantiate qt = return qt

skolemize :: Sigma -> Infer ([TyVar], Rho)
skolemize (TForall vars ty) = do -- Rule PRPOLY
    sks1 <- mapM (const $ fresh Star) vars
    let frame = M.fromList $ zip vars (fmap TVar sks1)
    (sks2, ty') <- skolemize (substitute frame ty)
    return (sks1 ++ sks2, ty')

skolemize (TFun tys) = do -- Rule PRFUN
    let tys_len = length tys
    let (arg_tys, [res_ty]) = splitAt (tys_len - 1) tys
    (sks, res_ty') <- skolemize res_ty
    return (sks, TFun $ arg_tys ++ [ res_ty' ])

skolemize ty = do -- Rule PRMONO
    return ([], ty)

-- | Constraint generation and type generation
infer :: AnnotatedExpr () -> Infer Type

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
            qt@(TForall _ (ps :=> ty)) <- instantiate scheme
            tyInst ps
            return ty

-- | A lambda abstraction is a list of symbols and an 'AnnotatedExpr' body. Each
-- argument should generate a unique 'Sigma' and the 'TypeEnv' should be
-- temporarily extended with them to evaluate the body.
infer (_ :< FunC args body) = do
    (argVars, argScms) <- mapAndUnzipM (\arg -> do
        var <- fresh Star >>= return . TVar
        return (var, TForall [] ([] :=> var))) args
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
