module Lib
    ( -- * Compiler
      Compiler
    , compile
    , compileWithLogs
      -- * Parsing & Syntax
    , parse_expr
    , parse_multi
    , removeComments
    , AnnotatedExpr
    , annotated
    , TopLevel(..)
    , Symbol
      -- * Preprocessing
    , Preprocess(..)
    , preprocess
    , uniqueIds
    , surfaceToTopLevel
    , splitUp
    , boundVarCheck
      -- * Type checking
    , typecheck
    , TypeEnv(..)
    , extendEnv
    , envLookup
    , defaultTypeEnv
    , emptyTypeEnv
    , buildTypeEnv
    , Scheme(..)
      -- * Lifted Expressions
    , LiftedExpr(..)
    , liftExpr
      -- * Codegen
    , codegen
    , emptyModule
      -- * Errors
    , PsiloError(..)
    ) where

import Lib.Parser
import Lib.Syntax
import Lib.Compiler
import Lib.Preprocessor
import Lib.Errors
import Lib.Types
import Lib.Util
import Lib.Codegen
