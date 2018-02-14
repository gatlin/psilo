module Lib.Types.Scheme where

import Prelude hiding (lookup)
import qualified Prelude as Prelude
import Lib.Syntax.Symbol
import Lib.Types.Kind
import Lib.Types.Type (TyVar(..), Type(..))
import Lib.Types.Qual
import Lib.Types.Frame

import Data.List (nub, lookup, intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

-- | A polymorphic, universally quantified type at the top-level scope
data Scheme = Forall [TyVar] (Qual Type) deriving (Eq, Ord)

-- | Substitute type variables in a 'Scheme' with variables starting at 0
normalize :: Scheme -> Scheme
normalize (Forall _ (ps :=> t)) = Forall (map snd ord) (ps' :=> (normtype t))
    where
        ord = zip (nub $ fv t) (map (\n -> TyVar n Star) [0..])

        find_pred (IsIn sym (TVar t)) = maybe
                                        (error $ "non-existent type variable: " ++ (show t))
                                        (\x -> (IsIn sym (TVar x)))
                                        (Prelude.lookup t ord)

        find_pred pred = pred
        ps' = map find_pred ps
        fv = reverse . S.toList . ftv

        normtype (TFun ts) = TFun $ map normtype ts
        normtype (TSym sym) = TSym sym
        normtype (TVar tv ) = case Prelude.lookup tv ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"


instance Show Scheme where
    show (Forall vars t) = prefix ++ (show t)
        where vars' = intercalate " " $ map show vars
              prefix = if (length vars) > 0 then "∀ " ++ vars' ++ ". "
                                            else ""

instance TypeLike Scheme where
    ftv (Forall vars t) = (ftv t) `S.difference` (S.fromList vars)

    substitute frame (Forall vars t) = Forall vars $
        substitute (foldr M.delete frame vars) t
