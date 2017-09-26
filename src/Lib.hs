module Lib
    (
      -- * Parsing & Syntax
      parse_expr
    , parse_multi
    , removeComments
    , AnnotatedExpr
    , annotated
    , TopLevel(..)
      -- * Preprocessing
    , Preprocess(..)
    , preprocess
    , uniqueIds
    , surfaceToTopLevel
      -- * Type checking
    , typecheck_defn
    , typecheck_defns
    , TypeEnv(..)
    , extendEnv
    , envLookup
    , defaultTypeEnv
      -- * Errors
    , PsiloError(..)
    ) where

import Lib.Parser
import Lib.Syntax
import Lib.Compiler.StackVM
import Lib.Preprocessor
import Lib.Errors
import Lib.Types
