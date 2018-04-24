module Lib.Types.Scheme where

import           Lib.Syntax.Symbol
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Qual
import           Lib.Types.Type    (TyVar (..), Type (..))
import           Prelude           hiding (lookup)
import qualified Prelude           as Prelude

import           Data.List         (intercalate, lookup, nub)
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Set          (Set)
import qualified Data.Set          as S

-- | A polymorphic, universally quantified type at the top-level scope.
-- The main use for a Scheme at this point is the normalization process, which
-- isn\'t even working correctly right now.
-- In theory the benefit of a Scheme is that, when normalized, two Schemes which
-- use different type variables but are isomorphic can be compared.
data Scheme = Scheme (Qual Type) deriving (Eq, Ord)

normalize :: Scheme -> Scheme
normalize (Scheme (ps :=> (TForall vs t))) = Scheme (ps' :=> (TForall vs' t'))
    where
        len_vs = (length vs) - 1
        vs' = map (\n -> TyVar n Star) [0..len_vs]
        frame = M.fromList $ zip vs (map TVar vs')
        ps' = substitute frame ps
        t' = substitute frame t

-- this will *probably* bite me in the ass later

instance Show Scheme where
    show (Scheme t) = show t

instance TypeLike Scheme where
    ftv (Scheme t) = (ftv t)

    substitute frame (Scheme t) = Scheme $
        substitute frame t
