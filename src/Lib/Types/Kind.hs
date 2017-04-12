module Lib.Types.Kind
where

data Kind
    = Star
    | Kfun Kind Kind
    deriving (Eq)

instance Show Kind where
    show (Star) = "*"
    show (Kfun a b) = show a ++ " -> " ++ show b
