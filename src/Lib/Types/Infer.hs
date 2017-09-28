{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types.Infer where

import Lib.Syntax.Symbol
import Lib.Syntax.Annotated
import Lib.Syntax.Core

import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Qual
import Lib.Types.Frame
import Lib.Types.Scheme
import Lib.Types.TypeEnv
import Lib.Types.Constraint

import Lib.Errors

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Except
import Data.Functor.Identity
import Data.Either (either)

import Control.Comonad
import Control.Comonad.Cofree

-- | The state we need to mutate during type inference
data InferState = InferState
    { varCount :: Int -- ^ monotonically increasing type ID number
    , typeEnv :: TypeEnv
    }

initInferState :: InferState
initInferState = InferState 0 mempty

newtype Infer a = Infer {
    unInfer
        :: RWST TypeEnv [Constraint] InferState (Except PsiloError) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState InferState
               , MonadReader TypeEnv
               , MonadWriter [Constraint]
               , MonadError PsiloError
               )

runInfer
    :: TypeEnv
    -> InferState
    -> Infer  a
    -> Except PsiloError (a, InferState, [Constraint])
runInfer te inferState (Infer m) = runRWST m te (inferState { typeEnv = te })

-- | helper to record an equality constraint
(@=) :: Type -> Type -> Infer ()
t1 @= t2 = tell [t1 := t2]

tyInst :: [Pred] -> Infer ()
tyInst [] = return ()
tyInst ps = forM_ (ftv ps) $ \tv -> tell [TVar tv :~ ps]

-- | temporarily extend the type environment for lambda abstraction

withEnv :: [(Symbol, Scheme)] -> Infer a -> Infer a
--withEnv xs m = local ((buildTypeEnv xs) <>) m
withEnv xs m = do
    modify $ \st -> st {
        typeEnv = (buildTypeEnv xs) <> (typeEnv st)
        }
    m

getEnv :: Infer TypeEnv
getEnv = gets typeEnv -- ask

-- | Generate a fresh type variable with a specific 'Kind'
fresh :: Kind -> Infer TyVar
fresh k = do
    c <- gets varCount
    modify $ \st -> st { varCount = c + 1 }
    return $ TyVar c k

-- | Instantiate a type scheme into a qualified type
instantiate :: Scheme -> Infer (Qual Type)
instantiate (Forall vs t) = do
    vs' <- mapM (const $ fresh Star) vs >>= mapM (return . TVar)
    let frame = M.fromList $ zip vs vs'
    return $ substitute frame t

-- | Constraint generation and type generation
infer :: AnnotatedExpr a -> Infer Type

-- constants are straightforward, except integers could be any @Num@ instance
infer (_ :< IntC _) = do
    ty <- fresh Star >>= \tv -> return $ TVar tv
    tyInst [IsIn "Num" ty]
    return ty

infer (_ :< BoolC _) = return $ typeBool
infer (_ :< FloatC _) = return $ typeFloat

-- Symbols are either not in the type environment, raising an exception, or they
-- map to a 'Scheme' which may be instantiated. In this way we allow
-- let-polymorphism for top-level definitions.
infer (_ :< IdC sym) = do
    tEnv <- getEnv
    var <- fresh Star >>= return . TVar
    case envLookup tEnv sym of
        Nothing -> return var
        Just scheme -> do
            qt@(ps :=> ty) <- instantiate scheme
            var @= ty
            tyInst ps
            return var

-- | A lambda abstraction is a list of symbols and an 'AnnotatedExpr' body. Each
-- argument should generate a unique 'Scheme' and the 'TypeEnv' should be
-- temporarily extended with them to evaluate the body.
infer (_ :< FunC args body) = do
    argVars <- forM args $ const (fresh Star >>= return . TVar)
    argScms <- forM argVars $ \v -> do
        return $ Forall [] ([] :=> v)
    br <- withEnv (zip args argScms) $ infer body
    let fun_ty = TFun $ argVars ++ [br]
    return fun_ty

-- | Lambda application is straightforward: infer types for the operator and the
-- operands, generate a fresh type variable for the return value, and assert
-- athat the operator is equivalent to a function consuming the operands and
-- producing the return value.
infer (_ :< AppC op erands) = do
    op' <- infer op
    erands' <- mapM infer erands
    var <- fresh Star >>= return . TVar
    --op' @= (TFun $ erands' ++ [var])
    recordArgs op' (erands' ++ [var])
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

recordArgs :: Type -> [Type] -> Infer ()
recordArgs (TFun argtys) erands = forM_ (zip argtys erands) go
    where go (argty, erand) = argty @= erand

recordArgs ty erands = ty @= (TFun erands)
