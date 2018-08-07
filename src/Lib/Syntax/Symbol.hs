module Lib.Syntax.Symbol where

import           Data.Set (Set)
import qualified Data.Set as S

type Symbol = String

builtin_syms :: Set Symbol
builtin_syms = S.fromList
    [ "+", "*", "-", "/", "=?", "<", ">", "modulo", "not"
    -- some non-overloaded numeric functions
    , "int-add", "int-mul", "int-div", "int-sub"
    , "float-add", "float-mul", "float-div", "float-sub"
    ]

mangle :: Symbol -> Symbol
mangle sym
    | sym == "-" = "-"
    | otherwise = concatMap (\c -> if c == '-' then "__" else [c]) sym
