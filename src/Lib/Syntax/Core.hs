{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib.Syntax.Core where

import Lib.Syntax.Symbol
import Control.Monad.Free

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
    | IfC { ifCond :: a, ifThen :: a, ifElse :: a }
    | TailRecC { tailRecArgs :: [a] }
    deriving ( Functor
             , Foldable
             , Traversable
             , Show
             , Eq
             , Ord
             )

-- | The free monad of 'CoreAst' is a DSL which used by the type checker, code
-- generators, and other subsystems.
type CoreExpr = Free CoreAst

-- | A definition pairs a 'Symbol' with a 'CoreExpr'.
data Definition = Define Symbol (CoreExpr ())
    deriving (Show, Eq)
