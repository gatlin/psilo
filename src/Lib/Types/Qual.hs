module Lib.Types.Qual where

import Lib.Syntax.Symbol
import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Frame

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S

-- | A predicate is an assertion about a type
data Pred = IsIn Symbol Type deriving (Eq, Ord)

instance Show Pred where
    show (IsIn c t) = c ++ " " ++ (show t)

instance TypeLike Pred where
    substitute frame (IsIn i t) = IsIn i (substitute frame t)
    ftv (IsIn i t) = ftv t

-- | Qualification means being paired with a list of predicates
data Qual t = [Pred] :=> t deriving (Eq, Ord)

instance Show t => Show (Qual t) where
    show ([] :=> t) = show t
    show (ps :=> t) = "(" ++ ps' ++ ")" ++ " => " ++ show t
        where ps' = intercalate ", " $ map show ps

instance TypeLike t => TypeLike (Qual t) where
    substitute frame (ps :=> t) =
        (substitute frame ps) :=> (substitute frame t)

    ftv (ps :=> t) = (ftv ps) `S.union` (ftv t)
