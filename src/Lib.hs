module Lib
    ( -- * Compiler
      Compiler
    , compile
    , Log
    , compileWithLogs
    , logMsg
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
