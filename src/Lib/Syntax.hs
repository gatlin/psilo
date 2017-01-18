{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Syntax where

import Control.Monad.Free

type Symbol = String

data Ast a
    = NumC { numC :: Double }
    | BoolC { boolC :: Bool }
    | StringC { stringC :: String }
    | IdC { idC :: Symbol }
    | AppC { appFun :: a, appArgs :: [a] }
    | ClosC { closCArgs :: [Symbol], closCBody :: a }
    | IfC { ifCond :: a, ifThen :: a, ifElse :: a }
    deriving (Functor, Show)

type Expr = Free Ast

aNumber :: (MonadFree Ast m) => Double -> m a
aNumber d = liftF $ NumC d

aBool :: (MonadFree Ast m) => Bool -> m a
aBool b = liftF $ BoolC b

aString :: (MonadFree Ast m) => String -> m a
aString s = liftF $ StringC s

aId :: (MonadFree Ast m) => Symbol -> m a
aId s = liftF $ IdC s

aApp :: (MonadFree Ast m) => a -> [a] -> m a
aApp f a = liftF $ AppC f a

aClos :: (MonadFree Ast m) => [Symbol] -> a -> m a
aClos a b = liftF $ ClosC a b

aIf :: (MonadFree Ast m) => a -> a -> a -> m a
aIf c t e = liftF $ IfC c t e
