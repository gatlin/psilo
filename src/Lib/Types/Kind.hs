module Lib.Types.Kind where

-- | Kinds are types at the type level.
data Kind
    = Star
    | Kind :-> Kind
    deriving (Eq, Ord)

instance Show Kind where
    show (Star) = "*"
    show (a :-> b) = (show a) ++ " -> " ++ (show b)

-- | Anything which is assigned a 'Kind'
class HasKind t where
    kind :: t -> Kind
