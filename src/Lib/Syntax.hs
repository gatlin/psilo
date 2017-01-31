{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Syntax where

import Control.Monad.Free
import Data.Text (Text)
import qualified Data.Text as T

type Symbol = String

data CoreAst a
    = IntC { intC :: Integer }
    | DoubleC { doubleC :: Double }
    | BoolC { boolC :: Bool }
    | StringC { stringC :: Text }
    | IdC { idC :: Symbol }
    | AppC { appFun :: a, appArgs :: [a] }
    | ClosC { closCArgs :: [Symbol], closCBody :: a }
    | IfC { ifCond :: a, ifThen :: a, ifElse :: a }
    | DefC { defSym :: Symbol, defValue :: a }
    deriving (Functor, Show)

type CoreExpr = Free CoreAst

aInt :: (MonadFree CoreAst m) => Integer -> m a
aInt d = liftF $ IntC d

aDouble :: (MonadFree CoreAst m) => Double -> m a
aDouble d = liftF $ DoubleC d

aBool :: (MonadFree CoreAst m) => Bool -> m a
aBool b = liftF $ BoolC b

aString :: (MonadFree CoreAst m) => Text -> m a
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
