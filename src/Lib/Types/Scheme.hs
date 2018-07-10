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
-- This is an alias for 'Type' to make navigating the code clearer.
type Scheme = Type

-- | Invariant: Schemes take the form 'forall [var...*] ([pred...*] => t)'.
normalize :: Scheme -> Scheme
normalize (TForall [] t) = t
normalize (TForall vs t) = TForall vs $ normalize t'
    where
        len_vs = (length vs) - 1
        vs' = map (\n -> TyVar n Star) [0..len_vs]
        frame = M.fromList $ zip vs (map TVar vs')
        t' = pred_massage frame t

        pred_massage fr (preds :=> ty) =
            (nub (substitute fr preds)) :=> ty
        pred_massage fr ty = ty
normalize t = t
