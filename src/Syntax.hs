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

type Sym = String
data LambdaName = Named Sym | Anonymous
    deriving (Eq, Show, Ord)

data AST a
    = ALambda LambdaName a a
    | AList [a]
    | AApply a a
    | AInteger Integer
    | AFloat Double
    | ABoolean Bool
    | ASymbol Sym

deriving instance Show a => Show (AST a)
deriving instance Functor AST
deriving instance Foldable AST
deriving instance Traversable AST
deriving instance Eq a => Eq (AST a)
deriving instance Ord a => Ord (AST a)

type PExpr = Free AST

aLambda n = liftF $ ALambda n id id

instance Show a => Show (PExpr a) where
    show (Pure _)   = ""
    show (Free x) = "Mu ( " ++ show x ++ " ) "
