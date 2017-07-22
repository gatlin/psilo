module Lib.Syntax.TopLevel where

import Lib.Syntax.Symbol
import Lib.Types.Scheme
import Lib.Syntax.Core

-- | Top level syntax forms
data TopLevel
    = Define Symbol (CoreExpr ()) -- ^ A value definition
    | Signature Symbol Scheme -- ^ A top level type scheme
    deriving (Show, Eq)
