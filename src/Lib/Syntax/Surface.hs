{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib.Syntax.Surface where

import Lib.Syntax.Symbol
import Lib.Syntax.Core
import Lib.Types.Type
import Control.Monad.Free
import Control.Monad (join)

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
    | SigS { sigSymS :: Symbol , sigScheme :: Sigma }
    | TypeDefS { typedefSymS :: Symbol
               , typedefVarsS :: [TyVar]
               , typedefBodyS :: Sigma }
    | ClassDefS { classDefNameS :: Symbol
                , classDefVarsS :: [Type]
                , classDefPreds :: [Pred]
                , classDefMethodsS :: [(a, Maybe a)] -- method names
                }
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

aApp :: (MonadFree SurfaceAst m) => m a -> [m a] -> m a
aApp f a = join . liftF $ AppS f a

aFun :: (MonadFree SurfaceAst m) => [Symbol] -> m a -> [Maybe (m a)] -> m a
aFun a b tys = join . liftF $ FunS a b tys

aIf :: (MonadFree SurfaceAst m) => m a -> m a -> m a -> m a
aIf c t e = join . liftF $ IfS c t e

aDef :: (MonadFree SurfaceAst m) => Symbol -> m a -> m a
aDef s b = join . liftF $ DefS s b

aSig
    :: (MonadFree SurfaceAst m)
    => Symbol
    -> Type
    -> m a
aSig sym t = join . liftF $ SigS sym t

aTypeDef
    :: (MonadFree SurfaceAst m)
    => Symbol
    -> [TyVar]
    -> Sigma
    -> m a
aTypeDef sym vars body = join . liftF $ TypeDefS sym vars body

aClassDef
    :: (MonadFree SurfaceAst m)
    => Symbol
    -> [Type]
    -> [Pred]
    -> [(m a, Maybe (m a))]
    -> m a
aClassDef name vars preds methods = join . liftF $
    ClassDefS name vars preds methods
