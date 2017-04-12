module Lib.Types.Scheme
where

import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Pred
import Lib.Types.Subst

data Scheme = Forall [Kind] (Qual Type)
    deriving (Eq)

instance Show Scheme where
    show (Forall ks (ps :=> t)) = "∀ " ++ show ps ++ ". " ++ show t

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall ks qt) = tv qt

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
    where vs' = [ v | v <- tv qt, v `elem` vs ]
          ks = map kind vs'
          s = zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)
