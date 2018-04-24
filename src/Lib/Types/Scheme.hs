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

-- | A polymorphic, universally quantified type at the top-level scope
data Scheme = Scheme (Qual Type) deriving (Eq, Ord)

normalize_type (TForall vs t) = (ord, normtype t)
    where ord = zip vs (map (\n -> TyVar n Star) [0..])
          normtype (TFun ts) = TFun $ map normtype ts
          normtype (TSym sym) = TSym sym
          normtype (TVar tv) = case Prelude.lookup tv ord of
              Just x  -> TVar x
              Nothing -> error "type variable not in signature"
          normtype (TForall _ t') = normtype t'
--          fv = reverse . S.toList . ftv

-- | This function is vestigial and should probably just go. As it stands it\'s
-- not quite right but since it\'s only used for pretty-printing it is not in
-- the way, either.
normalize :: Scheme -> Scheme
normalize (Scheme (ps :=> ty)) = Scheme $ ps' :=> ty'
    where
        (ord, ty') = normalize_type ty
        find_pred (IsIn sym (TVar t)) =
            maybe (error $ "non-existent type variable: " ++ (show t))
            (\x -> (IsIn sym (TVar x)))
            (Prelude.lookup t ord)
        find_pred pred = pred
        ps' = map find_pred ps

instance Show Scheme where
    show (Scheme t) = show t

instance TypeLike Scheme where
    ftv (Scheme t) = (ftv t)

    substitute frame (Scheme t) = Scheme $
        substitute frame t
