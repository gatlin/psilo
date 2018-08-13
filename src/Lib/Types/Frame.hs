{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.Types.Frame where

import           Control.Monad.Except
import           Lib.Errors

import           Data.Monoid          (Monoid, (<>))
import           Lib.Syntax.Symbol
import           Lib.Types.Type       (TyLit (..), TyVar (..), Type (..),
                                       showType, tyFun)

import           Data.Map             (Map)
import qualified Data.Map.Lazy        as M

import           Data.Set             (Set)
import qualified Data.Set             as S

import           Data.List            (intercalate, intersect)

-- * Frames and Substitutions

type Frame = Map TyVar Type

instance Show Frame where
    show mp = intercalate "\n" $ fmap show $ M.toList mp

nullFrame :: Frame
nullFrame = M.empty

(|->) :: TyVar -> Type -> Frame
u |-> t = M.fromList $ [(u, t)]

compose :: Frame -> Frame -> Frame
f1 `compose` f2 = M.map (substitute f1) f2 `M.union` f1

merge :: Frame -> Frame -> Maybe Frame
f1 `merge` f2
    | agree = Just $ f1 `M.union` f2
    | otherwise = Nothing
    where
        agree = all (\v -> substitute f1 (TVar v) == substitute f2 (TVar v))
                (fmap fst $ M.toList $ f1 `M.intersection` f2)

-- | This name is not optimal: entities which contain 'Type' information that
-- can be updated with a 'Frame' and which potentially bind 'TyVar's in them.
class TypeLike t where
    substitute :: Frame -> t -> t
    ftv :: t -> Set TyVar

instance TypeLike a => TypeLike [a] where
    ftv = foldr (S.union . ftv) S.empty
    substitute = map . substitute

instance TypeLike Type where
    ftv (TForall vs t) = (ftv t) `S.difference` (S.fromList vs)
    ftv (TVar n)       = S.singleton n
    ftv (TList ts)     = foldl (<>) mempty $ fmap ftv ts
--    ftv (ps :=> t)     = (ftv ps) `S.union` (ftv t)
--    ftv (IsIn i t)     = ftv t
    ftv _              = mempty

    substitute frame (TForall vs t) = TForall vs $
        substitute (foldr M.delete frame vs) t
    substitute frame (TVar u) = case M.lookup u frame of
        Just t  -> t
        Nothing -> TVar u

    substitute frame (TList ts) = TList (substitute frame ts)
--    substitute frame (IsIn i t) = IsIn i (substitute frame t)
--    substitute frame (ps :=> t) =
--        (map (substitute frame) ps) :=> (substitute frame t)
    substitute frame t = t

quantify :: Type -> Type
quantify (TForall vs ty) = TForall vs ty
quantify ty = TForall vs ty where
    vs = S.toList $ ftv ty
