{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Syntax.Lifted
    ( LiftedExpr(..)
    , liftExpr
    )
where

import Lib.Syntax.Symbol (Symbol)
import Lib.Syntax.Annotated (AnnotatedExpr(..))
import Lib.Syntax.Core (CoreAst(..))
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.State

data LiftedExpr
    = FloatL Double
    | IdL Symbol
    | AppL Symbol [LiftedExpr]
    | FunL Symbol [Symbol] LiftedExpr
    deriving (Show, Eq)

data LiftState = LiftState
    { uniqueId :: Int
    , exprs :: [LiftedExpr]
    }

defaultLiftState :: LiftState
defaultLiftState = LiftState 0 []

type Lift = State LiftState

gensym :: Lift Int
gensym = do
    n <- gets uniqueId
    modify $ \s -> s { uniqueId = n + 1 }
    return n

freshName :: Symbol -> Lift Symbol
freshName sym = do
    n <- gensym
    return $ sym ++ "#" ++ (show n)

liftExpr ::  Symbol -> AnnotatedExpr a -> [LiftedExpr]
liftExpr sym =
    exprs . ((flip execState) defaultLiftState) . go (Just sym) where

    go _ (_ :< FloatC n) = return $ FloatL n
    go _ (_ :< IntC n) = return $ FloatL $ fromIntegral n
    go _ (_ :< BoolC b) = return $ FloatL $ if b then 1.0 else 0.0
    go _ (_ :< IdC s) = return $ IdL s
    go _ (_ :< AppC op erands) = do
        op' <- handleOperator op
        erands' <- forM erands (go Nothing)
        return $ AppL op' erands'
    go mSym (_ :< FunC args body) = do
        name <- case mSym of
            Just sym' -> return sym'
            Nothing -> freshName sym
        body' <- go Nothing body
        let funL = FunL name args body'
        exprs' <- gets exprs
        modify $ \s -> s {
            exprs = exprs' ++ [funL]
            }
        return funL

    handleOperator :: AnnotatedExpr a -> Lift Symbol
    handleOperator (_ :< IdC s) = return s
    handleOperator f@(_ :< FunC _ _) = do
        (FunL name args body) <- go Nothing f
        return name
    handleOperator _ = error "You goofed"
