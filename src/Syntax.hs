{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Syntax where

import Prelude hiding (sequence)
import Control.Monad.Free
import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, sequence)

type Symbol = String

data AST a
    = AUnit
    | AList [a]
    | AInteger Integer
    | ABoolean Bool
    | ASymbol Symbol
    | ALambda a a
    | AApply a a
    | ADefine Symbol a

deriving instance Show a => Show (AST a)
deriving instance Functor AST
deriving instance Foldable AST
deriving instance Traversable AST
deriving instance Eq a => Eq (AST a)
deriving instance Ord a => Ord (AST a)

type Expr = Free AST

instance Show a => Show (Expr a) where
    show (Pure _)   = ""
    show (Free x) = " ( " ++ show x ++ " ) "

expr2symlist :: [Expr a] -> [Symbol]
expr2symlist ((Free (ASymbol x)):xs) = [x] ++ (expr2symlist xs)
expr2symlist _                       = []
