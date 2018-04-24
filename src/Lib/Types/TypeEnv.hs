{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types.TypeEnv where

import           Lib.Syntax.Symbol
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Qual
import           Lib.Types.Scheme
import           Lib.Types.Type

import           Data.List         (sort)

import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Set          (Set)
import qualified Data.Set          as S

newtype TypeEnv = TypeEnv (Map Symbol Scheme) deriving (Show)

instance Monoid TypeEnv where
    mempty = TypeEnv M.empty
    (TypeEnv t1) `mappend` (TypeEnv t2) = TypeEnv $ M.union t1 t2

instance TypeLike TypeEnv where
    substitute frame (TypeEnv env) = TypeEnv $ M.map (substitute frame) env
    ftv (TypeEnv env) = ftv $ M.elems env

buildTypeEnv :: [(Symbol, Scheme)] -> TypeEnv
buildTypeEnv = foldl go emptyTypeEnv
    where go tyEnv sig = extendEnv tyEnv sig

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv M.empty

extendEnv :: TypeEnv -> (Symbol, Scheme) -> TypeEnv
extendEnv (TypeEnv env) (x, s) = TypeEnv $ M.insert x s env

typeof :: TypeEnv -> Symbol -> Maybe Scheme
typeof (TypeEnv env) name = M.lookup name env

envRemove :: TypeEnv -> Symbol -> TypeEnv
envRemove (TypeEnv env) var = TypeEnv $ M.delete var env

envLookup :: TypeEnv -> Symbol -> Maybe Scheme
envLookup (TypeEnv env) sym = M.lookup sym env

-- | Generalize a qualified type inta a type scheme in a given context
generalize :: TypeEnv -> Qual Type -> Scheme
generalize te (ps :=> t) = Scheme ((sort ps) :=> (TForall as t))
    where as = S.toList $ ftv t `S.difference` ftv te

-- | Neatly lift qualified types up into type scheme
closeOver :: Frame -> Qual Type -> Scheme
closeOver f qt = normalize $ generalize mempty (substitute f qt)
