Expression syntax
===

The AST is a non-recursive data type. To add recursion to psilo's syntax, I
could use the Mu combinator:

    newtype Mu f = Mu (f (Mu f))
    type Expr = Mu AST

However, it so happens that the Free monad has a very similar definition:

    data Free f a = Pure a | Free (f (Free f a))
    type Expr = Free AST

Additionally, using `Free` yields a monad which permits sophisticated
evaluation using a relatively straight-forward case-wise "run" function (see
the Evaluator module).

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
>
> module Syntax where
>
> import Prelude hiding (sequence)
> import Control.Monad.Free
> import Data.Foldable (Foldable, fold)
> import Data.Traversable (Traversable, sequence)

> type Symbol = String
>
> data AST a
>     = AInteger Integer
>     | ASymbol Symbol
>     | ALambda Symbol a
>     | AAdd    a
>     | AMult   a
>     | AApply a a
>     | AList [a]
>
> deriving instance Show a => Show (AST a)
> deriving instance Functor AST
> deriving instance Foldable AST
> deriving instance Traversable AST
> deriving instance Eq a => Eq (AST a)
> deriving instance Ord a => Ord (AST a)
>
> type Expr = Free AST
>
> instance Show a => Show (Expr a) where
>     show (Pure _)   = ""
>     show (Free x) = " ( " ++ show x ++ " ) "
