{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Syntax where

import Prelude hiding (sequence)
import Control.Monad.Free
import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, sequence)

{-
 - Expression syntax
 -}

type Symbol = String

data AST a
    = AInteger Integer
    | ASymbol Symbol
    | ALambda Symbol a
    | AAdd    a
    | AMult   a
    | AApply a a
    | AList [a]

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
