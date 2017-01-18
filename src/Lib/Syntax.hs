{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Syntax where

import Control.Monad.Free

type Symbol = String

data CoreAst a
    = NumC { numC :: Double }
    | BoolC { boolC :: Bool }
    | StringC { stringC :: String }
    | IdC { idC :: Symbol }
    | AppC { appFun :: a, appArgs :: [a] }
    | ClosC { closCArgs :: [Symbol], closCBody :: a }
    | IfC { ifCond :: a, ifThen :: a, ifElse :: a }
    | DefC { defSym :: Symbol, defValue :: a }
    deriving (Functor, Show)

type CoreExpr = Free CoreAst

aNumber :: (MonadFree CoreAst m) => Double -> m a
aNumber d = liftF $ NumC d

aBool :: (MonadFree CoreAst m) => Bool -> m a
aBool b = liftF $ BoolC b

aString :: (MonadFree CoreAst m) => String -> m a
aString s = liftF $ StringC s

aId :: (MonadFree CoreAst m) => Symbol -> m a
aId s = liftF $ IdC s

aApp :: (MonadFree CoreAst m) => a -> [a] -> m a
aApp f a = liftF $ AppC f a

aClos :: (MonadFree CoreAst m) => [Symbol] -> a -> m a
aClos a b = liftF $ ClosC a b

aIf :: (MonadFree CoreAst m) => a -> a -> a -> m a
aIf c t e = liftF $ IfC c t e

aDef :: (MonadFree CoreAst m) => Symbol -> a -> m a
aDef s b = liftF $ DefC s b
