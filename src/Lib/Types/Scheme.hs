module Lib.Types.Scheme where

import           Lib.Syntax.Symbol
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Type    (Sigma, TyVar (..), Type (..),
                                    removeEmptyPreds)
import           Prelude           hiding (lookup)
import qualified Prelude           as Prelude

import           Data.List         (intercalate, lookup, nub)
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Set          (Set)
import qualified Data.Set          as S

-- | This module was named back when only rank-1 types were considered and a
-- "type scheme" alias was defined here. The normalization function is still
-- useful and, since it depends on Lib.Types.Type and Lib.Types.Frame, goes well
-- in its own module. Look, one thing at a time.

-- | Ideally, after normalization we can determine if two types are the same

normalize :: Sigma -> Sigma
normalize (TForall [] t) = normalize t
normalize (TForall vs t) = TForall (S.toList vs') $ normalize t'
    where
        vs' = (ftv t) `S.difference` (S.fromList vs)
        len_vs = (length $ S.toList vs') - 1
        vs'' = map (\v -> TyVar v Star) [0..len_vs]
        frame = M.fromList $ zip vs (map TVar vs'')
        t' = substitute frame (removeEmptyPreds t)
normalize t = removeEmptyPreds t
