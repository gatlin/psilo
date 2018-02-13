module Lib.Syntax.TopLevel where

import Lib.Syntax.Symbol
import Lib.Types.Scheme
import Lib.Syntax.Core
import Lib.Syntax.Annotated

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
    | Signature Symbol Scheme -- ^ A top level type scheme
    deriving (Show, Eq)
