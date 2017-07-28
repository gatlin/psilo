{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib.Syntax.Surface where

import Lib.Syntax.Symbol
import Lib.Syntax.Core
import Control.Monad.Free

-- * Surface Syntax

data TypeLit = TyConLit String | TyVarLit String
    deriving (Eq, Ord, Show)

fromTypeLit :: TypeLit -> String
fromTypeLit (TyConLit s) = s
fromTypeLit (TyVarLit s) = s

-- | Surface level AST
-- This is the AST of surface level syntax that actual code is written in. It is
-- separate from the core syntax so that superficial changes (including those
-- eventually performed by macros) won't affect the other subsystems, like the
-- type checker or code generator.

data SurfaceAst a
    = IntS { intS :: Integer }
    | FloatS { floatS :: Double }
    | BoolS { boolS :: Bool }
    | IdS { idS :: Symbol }
    | AppS { appFunS :: a, appArgsS :: [a] }
    | FunS { funArgsS :: [Symbol], funBodyS :: a, funSigS :: [Maybe a] }
    | IfS { ifCondS :: a, ifThenS :: a, ifElseS :: a }
    | DefS { defSymS :: Symbol, defValueS :: a }
    | SigS { sigSymS :: Symbol
           , sigVarS :: [TypeLit]
           , sigTypeS :: ([(Symbol, TypeLit)], [[TypeLit]]) }
    deriving ( Functor
             , Foldable
             , Traversable
             , Show
             , Eq
             , Ord
             )

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

aFun :: (MonadFree SurfaceAst m) => [Symbol] -> a -> [Maybe a] -> m a
aFun a b tys = liftF $ FunS a b tys

aIf :: (MonadFree SurfaceAst m) => a -> a -> a -> m a
aIf c t e = liftF $ IfS c t e

aDef :: (MonadFree SurfaceAst m) => Symbol -> a -> m a
aDef s b = liftF $ DefS s b

aSig
    :: (MonadFree SurfaceAst m)
    => Symbol
    -> [TypeLit]
    -> ([(Symbol, TypeLit)], [[TypeLit]])
    -> m a
aSig sym v t = liftF $ SigS sym v t
