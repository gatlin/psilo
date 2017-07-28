module Lib.Compiler.Preprocessor where

import Lib.Syntax ( Symbol
                  , SurfaceExpr
                  , SurfaceAst(..)
                  , CoreExpr
                  , CoreAst(..)
                  , TypeLit(..)
                  , fromTypeLit
                  )

import Lib.Types.Type (TyVar(..), Type(..))
import Lib.Types.Qual
import Lib.Types.TypeEnv
import Lib.Types.Scheme
import Lib.Types.Class
import Lib.Types (typecheck_defns)

import Control.Monad.Free
import Control.Comonad.Cofree

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

-- | The pre-processor takes 'SurfaceExpr's does the following
-- 1. renames symbols
