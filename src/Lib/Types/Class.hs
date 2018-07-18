{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types.Class where

import           Lib.Syntax.Symbol
import           Lib.Types.Kind        (HasKind, Kind (..))
import           Lib.Types.Type        (Pred, TyLit (..), TyVar (..), Type (..))
import           Lib.Types.TypeCheck

import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Monoid

import           Data.Functor.Identity

import           Lib.Compiler
import           Lib.Errors

-- | A typeclass instance is a qualified predicate
type Inst = Type

-- | A typeclass carries information about its superclasses and instances
type Class = ([Symbol], [Inst])

-- | Organizes information about the typeclass environment
newtype ClassEnv = ClassEnv (Map Symbol Class)
    deriving (Show, Monoid)

-- | Extracts typeclass information from a 'ClassEnv', if available.
classes :: ClassEnv -> Symbol -> Maybe Class
classes (ClassEnv ce) sym = M.lookup sym ce

-- These two functions assume the class actually exists. Use carefully.
super :: ClassEnv -> Symbol -> [Symbol]
super ce i = case classes ce i of Just (is, its) -> is

insts :: ClassEnv -> Symbol -> [Inst]
insts ce i = case classes ce i of Just (is, its) -> its

-- | Insert / Update a mapping in a 'ClassEnv'
modifyCE :: ClassEnv -> Symbol -> Class -> ClassEnv
modifyCE (ClassEnv ce) sym klass = ClassEnv $ M.insert sym klass ce

-- | For code readability
defined :: Maybe Class -> Bool
defined = isJust

-- | Represents a 'ClassEnv' transformation
newtype EnvTransformer = EnvT {
    transformCE :: ClassEnv -> Compiler ClassEnv }

-- | Combine 'EnvTransformer's
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(EnvT f) <:> (EnvT g) = EnvT $ \ce -> do
    ce' <- f ce
    g ce'
infixr 5 <:>

-- | I have no idea if this is useful
instance Monoid EnvTransformer where
    mempty = EnvT return
    mappend = (<:>)

-- | Adds a new typeclass to a 'ClassEnv', ensuring the name is not already
-- taken and that the superclasses are already defined.
addClass :: Symbol -> [Symbol] -> EnvTransformer
addClass sym is = EnvT go where
    go ce | defined (classes ce sym) = throwError $ ClassAlreadyDefined sym
          | any (not . defined . classes ce) is =
                throwError $ SuperclassNotDefined sym
          | otherwise = return (modifyCE ce sym (is, []))

-- | Adds an instance to a class, ensuring it does not overlap with others and
-- that the class exists.
addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn sym t) = EnvT go where
    go ce | not (defined (classes ce sym)) = throwError $ NoClassForInstance sym (show t)
          | any (overlap p) qs = throwError $ OverlappingInstance sym
          | otherwise = return $ modifyCE ce sym c
          where its = insts ce sym
                qs = [ q | (_ :=> q) <- its ]
                c = (super ce sym, (ps :=> p) : its)

-- | Make sure two predicates do not overlap
overlap :: Pred -> Pred -> Bool
overlap (IsIn _ t1) (IsIn _ t2) = case runSolve u st of
    Left _  -> False
    Right _ -> True
    where st = initTypeCheckState
          u :: TypeCheck Unifier
          u = unify t1 t2
