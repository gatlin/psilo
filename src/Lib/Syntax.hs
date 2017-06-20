{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib.Syntax where

import Control.Monad.Free
import Data.Text (Text)
import qualified Data.Text as T
import Control.Comonad
import Control.Comonad.Cofree
import Data.Set (Set)
import qualified Data.Set as S

-- * TODO
-- Different phases of compilation really need different syntax trees. For now
-- things are simple enough that I can, eg, make sure no 'TailRecC' values go
-- into the type checker. At some point, however, it'll be necessary to define
-- different grammars for different purposes.

type Symbol = String

builtin_syms :: Set Symbol
builtin_syms = S.fromList
    [ "+", "*", "-", "/", "=", "<", ">" ]

-- * Surface syntax construction

-- | Surface level syntax
-- The result of parsing is a surface level syntax which is reduced to a
-- 'CoreExpr' for use in type checking, codegen, etc. The reason for the
-- separation is so that complexities in surface-level syntax (such as new
-- special forms as yet to be defined) won't require drastic changes to the
-- other sub-systems.
data SurfaceAst a
    = IntS { intS :: Integer }
    | FloatS { floatS :: Double }
    | BoolS { boolS :: Bool }
    | IdS { idS :: Symbol }
    | AppS { appFunS :: a, appArgsS :: [a] }
    | FunS { funArgsS :: [Symbol], funBodyS :: a }
    | IfS { ifCondS :: a, ifThenS :: a, ifElseS :: a }
    | DefS { defSymS :: Symbol, defValueS :: a }
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

type SurfaceExpr = Free SurfaceAst

-- ** Convenience constructors for 'SurfaceExpr' values

aInt :: (MonadFree SurfaceAst m) => Integer -> m a
aInt d = liftF $ IntS d

aFloat :: (MonadFree SurfaceAst m) => Double -> m a
aFloat d = liftF $ FloatS d

aBool :: (MonadFree SurfaceAst m) => Bool -> m a
aBool b = liftF $ BoolS b

--aString :: (MonadFree CoreAst m) => Text -> m a
--aString s = liftF $ StringC s

aId :: (MonadFree SurfaceAst m) => Symbol -> m a
aId s = liftF $ IdS s

aApp :: (MonadFree SurfaceAst m) => a -> [a] -> m a
aApp f a = liftF $ AppS f a

aFun :: (MonadFree SurfaceAst m) => [Symbol] -> a -> m a
aFun a b = liftF $ FunS a b

aIf :: (MonadFree SurfaceAst m) => a -> a -> a -> m a
aIf c t e = liftF $ IfS c t e

aDef :: (MonadFree SurfaceAst m) => Symbol -> a -> m a
aDef s b = liftF $ DefS s b

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
--    | DefC { defSym :: Symbol, defValue :: a }
    | TailRecC { tailRecArgs :: [a] }
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

-- | The free monad of a 'CoreAst' is a DSL which may aid in constructing psilo
-- expressions for code generation.
type CoreExpr = Free CoreAst

-- | A definition pairs a 'Symbol' with a 'CoreExpr'.
data Definition = Define Symbol (CoreExpr ())
    deriving (Eq, Show)

type AnnotatedExpr = Cofree CoreAst

annotated :: CoreExpr () -> AnnotatedExpr ()
annotated (Free m) = () :< fmap annotated m
