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
import           Lib.Types.TypeCheck
import           Lib.Types.TypeEnv

import           Lib.Compiler
import           Lib.Errors

import           Data.List              (nub)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Comonad
import           Control.Comonad.Cofree

runInfer
    :: TypeEnv
    -> TypeCheck a
    -> Compiler (a)
runInfer te m = runInfer' te initTypeCheckState m

runInfer'
    :: TypeEnv
    -> TypeCheckState
    -> TypeCheck a
    -> Compiler (a)
runInfer' te tcState m = do
    ((a, tcState'), cs) <- runWriterT $
        runStateT (runReaderT m te) tcState
    return a

-- | helper to record an equality constraint
(@=) :: Type -> Type -> TypeCheck ()
t1 @= t2 = modify $ \st -> st {
    constraints = [t1 := t2] ++ (constraints st)
    }

(@~) :: Pred -> Type -> TypeCheck ()
p @~ t = modify $ \st -> st {
    constraints = [p :~ t] ++ (constraints st)
    }

tyInst :: [Pred] -> TypeCheck ()
tyInst [] = return ()
tyInst ps = forM_ ps $ \pred -> do
    let tvs = S.toList . ftv $ pred
    forM_ tvs $ \tv -> pred @~ (TVar tv)

withEnv :: [(Symbol, Sigma)] -> TypeCheck a -> TypeCheck a
withEnv ss m = local (<> (buildTypeEnv ss)) m

getEnv :: TypeCheck TypeEnv
getEnv = ask

-- | Instantiate a type scheme into a qualified type
instantiate :: Sigma -> TypeCheck Type
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
infer :: AnnotatedExpr () -> TypeCheck Type

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
