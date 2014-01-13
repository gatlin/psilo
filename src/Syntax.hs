module Syntax where

type Symbol = String

data PExpr
    = Float Double
    | Integer Integer
    | Boolean Bool
    | List [PExpr]
    | Quoted [PExpr]
    | Symbol String
    | Function Symbol [Symbol] PExpr
    deriving (Eq, Ord, Show)

