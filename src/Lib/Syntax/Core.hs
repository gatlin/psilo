{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib.Syntax.Core where

import Lib.Syntax.Symbol
import Control.Monad.Free
import Control.Monad (join)

-- | The psilo core syntax tree
-- Expressions represented in terms of 'CoreAst' have been parsed from surface
-- syntax, all macro transformations have been applied, and they are ready to be
-- given to other sub-systems.
data CoreAst a
    = IntC { intC :: Integer }
    | FloatC { floatC :: Double }
    | BoolC { boolC :: Bool }
    | IdC { idC :: Symbol }
    | AppC { appFun :: a, appArgs :: [a] }
    | FunC { funCArgs :: [Symbol], funCBody :: a }
    | IfC { ifCond :: a, ifThen :: a, ifElse :: a } -- not for long
    deriving ( Functor
             , Foldable
             , Traversable
             , Show
             , Eq
             , Ord
             )

cInt :: (MonadFree CoreAst m) => Integer -> m a
cInt n = liftF $ IntC n

cFloat :: (MonadFree CoreAst m) => Double -> m a
cFloat n = liftF $ FloatC n

cBool :: (MonadFree CoreAst m) => Bool -> m a
cBool b = liftF $ BoolC b

cId :: (MonadFree CoreAst m) => Symbol -> m a
cId s = liftF $ IdC s

cApp :: (MonadFree CoreAst m) => m a -> [m a] -> m a
cApp f a = join . liftF $ AppC f a

cFun :: (MonadFree CoreAst m) => [Symbol] -> m a -> m a
cFun a b = join . liftF $ FunC a b

cIf :: (MonadFree CoreAst m) => m a -> m a -> m a -> m a
cIf c t e = join . liftF $ IfC c t e

-- | The free monad of 'CoreAst' is a DSL which used by the type checker, code
-- generators, and other subsystems.
type CoreExpr = Free CoreAst
