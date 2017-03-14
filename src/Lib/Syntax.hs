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

-- * TODO
-- Different phases of compilation really need different syntax trees. For now
-- things are simple enough that I can, eg, make sure no 'TailRecC' values go
-- into the type checker. At some point, however, it'll be necessary to define
-- different grammars for different purposes.

type Symbol = String

-- | The psilo core syntax tree
-- Expressions represented in terms of 'CoreAst' have been parsed from surface
-- syntax, all macro transformations have been applied, and they are ready to be
-- given to code generators.
--
-- For the sake of efficiency some branches exist to aid with parsing and code
-- generation. For instance, in addition to bare identifiers ('IdC') a few
-- special case terms exist for symbols which are reserved for other purposes,
-- such as fixed point numbers ('IntC'), floating point numbers ('DoubleC'),
-- boolean values ('BoolC'), etc.
--
-- For code generators which support it the 'TailRecC' term also exists. It is
-- simple enough for a code generator which does not support tail recursion
-- elimination to replace such terms with 'AppC' terms.
data CoreAst a
    = IntC { intC :: Integer }
    | DoubleC { doubleC :: Double }
    | BoolC { boolC :: Bool }
--    | StringC { stringC :: Text }
    | IdC { idC :: Symbol }
    | AppC { appFun :: a, appArgs :: [a] }
    | FunC { funCArgs :: [Symbol], funCBody :: a }
    | IfC { ifCond :: a, ifThen :: a, ifElse :: a }
    | DefC { defSym :: Symbol, defValue :: a }
    | TailRecC { tailRecArgs :: [a] }
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

-- | The free monad of a 'CoreAst' is a DSL which may aid in constructing psilo
-- expressions for code generation.
type CoreExpr = Free CoreAst

aInt :: (MonadFree CoreAst m) => Integer -> m a
aInt d = liftF $ IntC d

aDouble :: (MonadFree CoreAst m) => Double -> m a
aDouble d = liftF $ DoubleC d

aBool :: (MonadFree CoreAst m) => Bool -> m a
aBool b = liftF $ BoolC b

--aString :: (MonadFree CoreAst m) => Text -> m a
--aString s = liftF $ StringC s

aId :: (MonadFree CoreAst m) => Symbol -> m a
aId s = liftF $ IdC s

aApp :: (MonadFree CoreAst m) => a -> [a] -> m a
aApp f a = liftF $ AppC f a

aFun :: (MonadFree CoreAst m) => [Symbol] -> a -> m a
aFun a b = liftF $ FunC a b

aIf :: (MonadFree CoreAst m) => a -> a -> a -> m a
aIf c t e = liftF $ IfC c t e

aDef :: (MonadFree CoreAst m) => Symbol -> a -> m a
aDef s b = liftF $ DefC s b
