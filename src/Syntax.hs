module Syntax where

type Sym = String

data PExpr
    = Float Double
    | Integer Integer
    | Boolean Bool
    | List [PExpr]
    | Quoted [PExpr]
    | Symbol Sym
    | Function Sym [Sym] PExpr
    | Let [PExpr] PExpr
    deriving (Eq, Ord, Show)

