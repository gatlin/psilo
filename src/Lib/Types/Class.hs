{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types.Class where

import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Lib.Syntax.Symbol
import           Lib.Types.Frame
import           Lib.Types.Kind        (HasKind, Kind (..))
import           Lib.Types.Type        (Pred, Qual, TyLit (..), TyVar (..),
                                        Type (..))

import           Data.Functor.Identity

import           Lib.Compiler
import           Lib.Errors

-- | Qualify a type with predicates.
-- Handle both cases: where there are existing predicates, and not.
-- NB: This will become more sophisticated when we have an actual class taxonomy
-- and some means of merging contexts.
qualify :: [Pred] -> Type -> Qual
qualify [] ty                 = ty
qualify preds (preds' :=> ty) = (preds ++ preds') :=> ty
qualify preds ty              = qualify preds $ [] :=> ty

-- | A class stores its variables, any "superclasses" (qualifications on its
-- type variables) and instances.
data Class = Class
    { vars      :: [Type]
    , supers    :: [Pred] -- Superclass / constraint information
    , instances :: [[Type]]
    } deriving (Show)

newtype ClassEnv = ClassEnv (Map Symbol Class) deriving (Show, Monoid)

classLookup :: ClassEnv -> Symbol -> Maybe Class
classLookup (ClassEnv ce) name = M.lookup name ce

-- | Insert / Update a mapping in a 'ClassEnv'
modifyCE :: ClassEnv -> Symbol -> Class -> ClassEnv
modifyCE (ClassEnv ce) sym klass = ClassEnv $ M.insert sym klass ce

-- | Alias for readibility.
defined :: Maybe Class -> Bool
defined = isJust

-- | Transforms class environments, allowing us to do quality control and
-- potentially throw exceptions along the way.
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

-- | Transforms a 'ClassEnv' by inserting a new 'Class'.
addClass :: Symbol -> [Type] -> [Pred] -> EnvTransformer
addClass name vars preds = EnvT go where
    go ce | defined (classLookup ce name) =
                throwError $ ClassAlreadyDefined name
          | any (not . defined . classLookup ce) predNames =
                throwError $ SuperclassNotDefined name
          | otherwise = return (modifyCE ce name (Class vars preds []))

    predNames = fmap (\(TPred sym _) -> sym) preds

{-
-- | A typeclass instance is a qualified predicate
type Inst = Type

-- These two functions assume the class actually exists. Use carefully.
super :: ClassEnv -> Symbol -> [Symbol]
super ce i = case classes ce i of Just (is, its) -> is

insts :: ClassEnv -> Symbol -> [Inst]
insts ce i = case classes ce i of Just (is, its) -> its

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
          u = unifyMany t1 t2
-}
