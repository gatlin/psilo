module Lib.Types.Kind where

data Kind
    = Star
    | Kind :-> Kind
    deriving (Eq, Ord)

instance Show Kind where
    show (Star) = "*"
    show (a :-> b) = (show a) ++ " -> " ++ (show b)

-- | Any thing which is assigned a 'Kind'
class HasKind t where
    kind :: t -> Kind
