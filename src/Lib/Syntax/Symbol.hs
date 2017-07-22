module Lib.Syntax.Symbol where

import Data.Set (Set)
import qualified Data.Set as S

type Symbol = String

builtin_syms :: Set Symbol
builtin_syms = S.fromList
    [ "+", "*", "-", "/", "=", "<", ">" ]
