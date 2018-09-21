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
    , showSignatures
    )
where

import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Lifted
import           Lib.Syntax.Surface
import           Lib.Syntax.Symbol
import           Lib.Syntax.TopLevel

import           Lib.Types.Class
import           Lib.Types.Kind
import           Lib.Types.Scheme
import           Lib.Types.Type
import           Lib.Types.TypeEnv      (TypeEnv (..), generalize)

import           Control.Comonad.Cofree
import           Control.Monad          (join)
import           Control.Monad.Free
import           Control.Monad.State
import           Data.Map               (Map)
import qualified Data.Map               as M
