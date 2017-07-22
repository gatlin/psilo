{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types.TypeEnv where

import Lib.Syntax.Symbol
import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Qual
import Lib.Types.Frame
import Lib.Types.Scheme

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

newtype TypeEnv = TypeEnv (Map Symbol Scheme) deriving (Monoid, Show)

instance TypeLike TypeEnv where
    substitute frame (TypeEnv env) = TypeEnv $ M.map (substitute frame) env
    ftv (TypeEnv env) = ftv $ M.elems env

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
generalize te t = Forall as t
    where as = S.toList $ ftv t `S.difference` ftv te

-- | Neatly lift qualified types up into type scheme
closeOver :: Frame -> Qual Type -> Scheme
closeOver f qt = normalize $ generalize mempty (substitute f qt)
