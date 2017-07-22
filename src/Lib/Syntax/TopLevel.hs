module Lib.Syntax.TopLevel where

import Lib.Syntax.Symbol
import Lib.Syntax.Core

-- | Top level syntax forms
data TopLevel
    = Define Symbol (CoreExpr ()) -- ^ A value definition
    deriving (Show, Eq)
