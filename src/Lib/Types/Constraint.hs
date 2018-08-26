module Lib.Types.Constraint where

import           Lib.Types.Frame
import           Lib.Types.Type

import           Data.Set        (Set)
import qualified Data.Set        as S

-- | An equality constraint for two types for type inference.
data Constraint
    = Type := Type
    | String :~ [Type]
    deriving (Eq, Ord, Show)

instance TypeLike Constraint where
    substitute frame c = case c of
        (t1 := t2) -> (substitute frame t1) := (substitute frame t2)
        (s  :~ ts) -> s :~ (substitute frame ts)

    ftv c = case c of
        (t1 := t2) -> (ftv t1) `S.union` (ftv t2)
        (s  :~ ts) -> ftv ts
