module Lib.Types.Constraint where

import Lib.Syntax (Symbol)
import Lib.Types.Type
import Lib.Types.Qual
import Lib.Types.Frame

import Data.Set (Set)
import qualified Data.Set as S

-- | Constraints used in type inference: equality constraints and instance
-- constraints. An equality constraint means that two types are equivalent and
-- will be unified with each other. An instance constraint means that a type
-- holds for a set of predicates.
data Constraint
    = Type := Type
    | Type :~ [Pred]
    | Type :$ Symbol
    deriving (Eq, Ord, Show)

instance TypeLike Constraint where
    substitute frame c = case c of
        (t1 := t2) -> (substitute frame t1) := (substitute frame t2)
        (tv :~ ps) -> (substitute frame tv) :~ (substitute frame ps)
        (ty :$ sm) -> (substitute frame ty) :$ sm

    ftv c = case c of
        (t1 := t2) -> (ftv t1) `S.union` (ftv t2)
        (tv :~ ps) -> (ftv tv) `S.union` (ftv ps)
        (ty :$ sm) -> (ftv ty)
