module Lib.Syntax.TopLevel where

import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol
import           Lib.Types.Scheme
import           Lib.Types.Type       (Sigma, TyVar (..))

-- | Top level syntax forms
-- Eventually we want to support the following:
--   Functions
--   Type signatures
--   Class Definitions
--   Class Instances
--   Type aliases
--   New data types
data TopLevel
    = Define Symbol (AnnotatedExpr ()) -- ^ A value definition
    | Signature Symbol Sigma -- ^ A top level type scheme
    | Typedef Symbol [TyVar] Sigma
    deriving (Show, Eq)
