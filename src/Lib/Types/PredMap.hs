{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types.PredMap where

import Lib.Syntax.Symbol
import Lib.Types.Type (TyVar(..), Type(..))
import Lib.Types.Qual
import Lib.Types.Frame
import Lib.Types.TypeLike

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

newtype PredMap = PMap (Map Type [Pred])
    deriving (Monoid, Show)

instance TypeLike PredMap where
    ftv (PMap m) = ftv (M.elems m)

    -- potentially several distinct type variables will be combined into the
    -- same new one, and thus we want to make sure to merge the various
    -- constraints we have deduced for them
    substitute frame pmap@(PMap m) = pmap' where
        keys = M.keys m
        keys_subbed = substitute frame keys
        key_fold pm (k, k') = updatePredMap
                              k'
                              (substitute frame (lookupPreds k pm))
                              pm
        pmap' = foldl key_fold pmap (zip keys keys_subbed)

-- | Insert (or merge) a list of predicates into a `PredMap`
updatePredMap :: Type -> [Pred] -> PredMap -> PredMap
updatePredMap ty ps (PMap m) = PMap $ case M.lookup ty m of
    Nothing -> M.insert ty ps m
    Just ps' -> M.insert ty (nub $ (ps ++ ps')) m

-- | Lookup the list of predicates associated with a type
lookupPreds :: Type -> PredMap -> [Pred]
lookupPreds ty (PMap m) = concat $ fmap go tvs where
    tvs = S.toList $ ftv ty
    go tv = case M.lookup (TVar tv) m of
        Nothing -> []
        Just ps -> ps
