module Lib.Types.Frame where

import Lib.Syntax.Symbol
import Lib.Types.Type (TyVar(..), Type(..))
import Lib.Types.TypeLike
import Data.Monoid (Monoid, (<>))

import Data.Map (Map)
import qualified Data.Map.Lazy as M

import Data.Set (Set)
import qualified Data.Set as S

-- * Frames and Substitutions

type Frame = Map TyVar Type

nullFrame :: Frame
nullFrame = M.empty

(|->) :: TyVar -> Type -> Frame
u |-> t = M.fromList $ [(u, t)]

compose :: Frame -> Frame -> Frame
f1 `compose` f2 = M.map (substitute f1) f2 `M.union` f1

class TypeLike t where
    substitute :: Frame -> t -> t
    ftv :: t -> Set TyVar

instance TypeLike a => TypeLike [a] where
    ftv = foldr (S.union . ftv) S.empty
    substitute = map . substitute

instance TypeLike Type where
    ftv (TVar n) = S.singleton n
    ftv (TFun ts) = foldl (<>) mempty $ fmap ftv ts
    ftv _ = mempty

    substitute frame (TVar u) = case M.lookup u frame of
        Just t -> t
        Nothing -> TVar u

    substitute frame (TFun ts) = TFun (substitute frame ts)
    substitute frame t = t
