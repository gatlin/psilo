module Lib.Types.TypeError where

import Lib.Syntax.Symbol
import Lib.Types.Type (TyVar(..), Type(..))

-- | The type of errors we might encounter during inference
data TypeError
    = UnificationFail Type Type
    | UnificationMismatch [Type] [Type]
    | InfiniteType TyVar Type
    | UnboundVariable Symbol
    | OtherTypeError String
    deriving (Eq, Show)
