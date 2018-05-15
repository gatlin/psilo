module Lib.Types.Scheme where

import           Lib.Syntax.Symbol
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Type    (Pred, TyVar (..), Type (..))
import           Prelude           hiding (lookup)
import qualified Prelude           as Prelude

import           Data.List         (intercalate, lookup, nub)
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Set          (Set)
import qualified Data.Set          as S

-- | A polymorphic, universally quantified type at the top-level scope.
-- In theory the benefit of a Scheme is that, when normalized, two Schemes which
-- use different type variables but are isomorphic can be compared.
type Scheme = Type

normalize :: Scheme -> Scheme
normalize (ps :=> (TForall vs t)) = (ps' :=> (TForall vs' t'))
    where
        len_vs = (length vs) - 1
        vs' = map (\n -> TyVar n Star) [0..len_vs]
        frame = M.fromList $ zip vs (map TVar vs')
        ps' = substitute frame ps
        t' = substitute frame t
