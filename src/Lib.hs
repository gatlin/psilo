module Lib
    (
      -- * Parsing & Syntax
      parse_expr
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
      -- * Codegen
    , run
    , codegen
    , stackPeek
    , newCodegenContext
    , newCodegenState
    , runCodegenT
    , MachineT(..)
    , MachineState(..)
      -- * Errors
    , PsiloError(..)
    ) where

import Lib.Parser
import Lib.Syntax
import Lib.Compiler.StackVM
import Lib.Preprocessor
import Lib.Errors
import Lib.Types
import Lib.Util
