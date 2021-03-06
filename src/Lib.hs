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
    , SurfaceExpr
    , CoreAst(..)
    , annotated
    , TopLevel(..)
    , showSignatures
    , Symbol
    , builtin_syms
      -- * Preprocessing
    , Preprocess(..)
    , preprocess
    , uniqueIds
    , surfaceToTopLevel
    , PState(..)
--    , splitUp
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
    , EnvTransformer(..)
    , (<:>)
    , ClassEnv(..)
    , showType
      -- * Errors
    , PsiloError(..)
    ) where

--import           Lib.Codegen
import           Lib.Compiler
import           Lib.Errors
import           Lib.Parser
import           Lib.Preprocessor
import           Lib.Syntax
import           Lib.Types
