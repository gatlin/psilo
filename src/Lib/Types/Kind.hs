module Lib.Types.Kind where

import           Data.List (intercalate)

data Kind
    = Star
    | KFun [Kind]
    deriving (Eq, Ord)

instance Show Kind where
    show (Star)    = "*"
    show (KFun ks) = "(" ++ (intercalate " -> " (fmap show ks)) ++ ")"
--    show (a :-> b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"

-- | Any thing which is assigned a 'Kind'
class HasKind t where
    kind :: t -> Kind
