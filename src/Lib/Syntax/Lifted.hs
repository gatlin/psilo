{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Lib.Syntax.Lifted
-- Description: Lambda-lifted syntax tree for code generation.
--
-- After type checking and other static analysis expressions are converted into
-- a lambda-lifted form. This module defines that grammar and the lambda lifting
-- operation(s).

module Lib.Syntax.Lifted
where

import Lib.Syntax.Symbol (Symbol, mangle, builtin_syms)
import Lib.Syntax.Annotated (AnnotatedExpr(..))
import Lib.Syntax.Core (CoreAst(..))
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.State
import Control.Monad.Free
import Data.ByteString.Short
import qualified Data.Set as S

-- | Lambda-lifted expression grammar
data LiftedExpr
    = FloatL Double
    | IdL Symbol
    | AppL Symbol [LiftedExpr]
    | FunL Symbol [Symbol] LiftedExpr
    deriving (Show, Eq)

-- | The state we manage while constructing a set of lifted expressions.
data LiftState = LiftState
    { uniqueId :: Int
    , exprs :: [LiftedExpr]
    }

defaultLiftState :: LiftState
defaultLiftState = LiftState 0 []

type Lift = State LiftState

-- | Generate a unique symbol using sophisticated mathematics
gensym :: Lift Symbol
gensym = do
    n <- gets uniqueId
    modify $ \s -> s { uniqueId = n + 1 }
    return $ "#" ++ (show n)

-- | Gin up a fresh name for newly-anointed anonymous functions
freshName :: Symbol -> Lift Symbol
freshName sym = do
    sym' <- gensym
    return $ sym ++ sym'

without :: Eq a => [a] -> [a] -> [a]
without = foldr (filter . (/=)) -- Like \\ but remove all occurrences

freeVars :: AnnotatedExpr a -> [Symbol]
freeVars (_ :< IdC s) = [s]
freeVars (_ :< AppC op erands) = (freeVars op) ++ (concatMap freeVars erands)
freeVars (_ :< FunC args body) = (freeVars body) `without` args
freeVars _ = []

applyTo :: AnnotatedExpr () -> [Symbol] -> AnnotatedExpr ()
applyTo e (s : ss) = applyTo (() :< (AppC e [(() :< IdC s)])) ss
applyTo e [] = e

convertClosure :: [Symbol] -> AnnotatedExpr () -> AnnotatedExpr ()
convertClosure globals expr = go expr where
    go (() :< FunC args body) =
        let vars = freeVars body `without` (globals ++ args)
        in  (() :< FunC (vars ++ args) body) `applyTo` vars

    go expr = expr

globals = S.toList builtin_syms

liftExpr ::  Symbol -> AnnotatedExpr () -> [LiftedExpr]
liftExpr sym =
    exprs . ((flip execState) defaultLiftState) . go' (Just $ mangle sym) where

    go' mSym e = go mSym $ convertClosure globals e
    go _ (_ :< FloatC n) = return $ FloatL n
    go _ (_ :< IntC n) = return $ FloatL $ fromIntegral n
    go _ (_ :< BoolC b) = return $ FloatL $ if b then 1.0 else 0.0
    go _ (_ :< IdC s) = return $ IdL $ mangle s
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
            exprs = funL : exprs'
            }
        return funL

    handleOperator :: AnnotatedExpr () -> Lift Symbol
    handleOperator (_ :< IdC s) = return s
    handleOperator f@(_ :< FunC _ _) = do
        funL@(FunL name args body) <- go Nothing f
        exprs' <- gets exprs
        modify $ \s -> s { exprs = funL : exprs' }
        return name
    handleOperator _ = error "You goofed"
