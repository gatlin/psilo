module Lib.Syntax.Symbol where

import           Data.Set (Set)
import qualified Data.Set as S

type Symbol = String

builtin_syms :: Set Symbol
builtin_syms = S.fromList
    [ "int-add", "int-mul", "int-div", "int-sub", "int-modulo"
    , "float-add", "float-mul", "float-div", "float-sub", "float-modulo"
    , "not"
    , "int=?"
    , "float=?"
    , "float-fromint"
    , "boolean-fromint"
    , "byte-and"
    , "byte-or"
    , "byte-not"
    , "byte-xor"
    , "byte-2c"
    , "eval"
    ]

{-
    [-- "=?", "<", ">", "modulo", "not"
    -- some non-overloaded numeric functions
    , "int-add", "int-mul", "int-div", "int-sub"
    , "float-add", "float-mul", "float-div", "float-sub"
    ]
-}

mangle :: Symbol -> Symbol
mangle sym
    | sym == "-" = "-"
    | otherwise = concatMap (\c -> if c == '-' then "__" else [c]) sym
