module Lib.Syntax
    ( Symbol
    , builtin_syms
    , CoreExpr
    , CoreAst(..)
    , TopLevel(..)
    , SurfaceExpr
    , SurfaceAst(..)
    , aInt
    , aFloat
    , aBool
    , aId
    , aApp
    , aFun
    , aIf
    , aDef
    , AnnotatedExpr
    , annotated
    , TypeLit(..)
    , fromTypeLit
    , LiftedExpr(..)
    , liftExpr
    )
where

import Lib.Syntax.Symbol
import Lib.Syntax.Core
import Lib.Syntax.Surface
import Lib.Syntax.Annotated
import Lib.Syntax.TopLevel
import Lib.Syntax.Lifted

import Lib.Types.Scheme
import Lib.Types.Qual
import Lib.Types.Type
import Lib.Types.Kind
import Lib.Types.TypeEnv (TypeEnv(..), generalize)

import Control.Monad (join)
import Control.Monad.Free
import Control.Monad.State
import Control.Comonad.Cofree
import Data.Map (Map)
import qualified Data.Map as M

-- | ROADMAP
-- 1. Convert each bound symbol into a unique symbol in expressions.
-- 2. Allow surface syntax to mix types and expressions.
-- 3. The conversion process should really be from surface to some composite
-- structure containing definitions, type judgments, and anything else an
-- evaluator would require.
-- 4. Actually generate "assembly" and execute programs.

-- | Converts a 'SurfaceExpr' to a 'CoreExpr' or fails.
-- TODO needs to convert each bound symbol into a unique symbol. Suggests a
-- reader monad.

-- This is a remnant of some old code I will use again and don't want to figure
-- out again
{-
        tailRec sym (Free (IfS c t e)) = Free $
            IfC (convert c) (tailRec sym t) (tailRec sym e)

        tailRec sym expr@(Free (AppS (Free (IdS fun)) ops))
            | sym == fun = Free $ TailRecC (map convert ops)
            | otherwise = convert expr

        tailRec _ other = convert other
-}
