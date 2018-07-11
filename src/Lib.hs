module Lib
    ( -- * Compiler
      Compiler
    , compile
    , compileWithLogs
    , logMsg
      -- * Parsing & Syntax
    , parse_expr
    , parse_multi
    , removeComments
    , AnnotatedExpr
    , CoreAst(..)
    , annotated
    , TopLevel(..)
    , Symbol
    , builtin_syms
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
    , normalize
      -- * Lifted Expressions
    , LiftedExpr(..)
    , liftExpr
    , convertClosure
      -- * Codegen
    , codegen
    , emptyModule
      -- * Errors
    , PsiloError(..)
    ) where

import           Lib.Codegen
import           Lib.Compiler
import           Lib.Errors
import           Lib.Parser
import           Lib.Preprocessor
import           Lib.Syntax
import           Lib.Types
import           Lib.Util
