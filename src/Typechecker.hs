{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Typechecker where

import Prelude hiding (sequence)

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Free
import Control.Monad.State hiding (sequence)
import Data.Foldable (Foldable, fold)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Traversable (Traversable, sequence)
import qualified Data.Map as M

import Syntax

data Type
    = Type :-> Type
    | TVar Int
    | TNumber
    | TBoolean
    | TVoid
    | TList [Type]
    | TError String

deriving instance Show Type

data Constraint = EqualityConstraint Type Type
deriving instance Show Constraint

data TypeResult = TypeResult
    { constraints :: [Constraint]
    , assumptions :: M.Map String [Type]
    }

deriving instance Show TypeResult

instance Monoid TypeResult where
    mempty = TypeResult { constraints = mempty
                        , assumptions = mempty }
    mappend a b = TypeResult
        { constraints = constraints a `mappend` constraints b
        , assumptions = assumptions a `mappend` assumptions b
        }

data TypeState t m = TypeState
    { varId :: Int
    , memo  :: M.Map t m
    }

freshVarId :: State (TypeState t m) Type
freshVarId = do
    v <- gets varId
    modify $ \s -> s { varId = succ v }
    return $ TVar v

type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

memoizedTC :: Ord c
           => (c -> TypeCheck c)
           -> c
           -> TypeCheck c
memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c where
    memoize = do
        r <- f c
        modify $ \s -> s { memo = M.insert c r $ memo s }
        return r

cofreeMu :: Functor f => Free f t -> Cofree f ()
cofreeMu (Free f) = () :< fmap cofreeMu f

attribute :: Cofree AST () -> Cofree AST (Type, TypeResult)
attribute c =
    let initial = TypeState { memo = M.empty, varId = 0 }
    in  evalState (sequence $ extend (memoizedTC generateConstraints) c)
                  initial

generateConstraints :: Cofree AST () -> TypeCheck (Cofree AST ())

generateConstraints (() :< AInteger _) = return (TNumber, mempty)
generateConstraints (() :< ABoolean _) = return (TBoolean, mempty)

generateConstraints (() :< ASymbol s) = do
    var <- freshVarId
    return (var, TypeResult
        { constraints = []
        , assumptions = M.singleton s [var]
        })

generateConstraints (() :< ALambda (_ :< args') b) = do
    var <- freshVarId
    let (AList args) = fmap (\(_ :< ASymbol sym) -> sym) args'
    br <- memoizedTC generateConstraints b
    let cs = mconcat $ map (\s -> maybe [] (map $ EqualityConstraint var)
                      (M.lookup s . assumptions $ snd br)) args
        as = mconcat $ map (\s -> M.delete s . assumptions $ snd br) args
    return (var :-> (fst br), TypeResult {
        constraints = constraints (snd br) <> cs,
        assumptions = as })

generateConstraints (() :< AApply a b) = do
    var <- freshVarId
    ar <- memoizedTC generateConstraints a
    br <- memoizedTC generateConstraints b
    return (var, snd ar <> snd br <> TypeResult {
        constraints = [EqualityConstraint (fst ar) $ (fst br) :-> var],
        assumptions = mempty })

generateConstraints (() :< AList items) = do
    var <- freshVarId
    return (var, TypeResult {
        constraints = mempty,
        assumptions = mempty })

generateConstraints (() :< ADefine sym val) = do
    valType <- memoizedTC generateConstraints val
    return (fst valType, TypeResult
        { constraints = constraints (snd valType)
        , assumptions = assumptions (snd valType)
        })

generateConstraints (() :< AUnit) = return (TVoid, mempty)

generateConstraints blah = return (TError (show blah), mempty)

solveConstraints :: [Constraint] -> Maybe (M.Map Int Type)
solveConstraints =
    foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty where
        solve maybeSubs (EqualityConstraint a b) = do
            subs <- maybeSubs
            mostGeneralUnifier (substitute subs a) (substitute subs b)

mostGeneralUnifier :: Type -> Type -> Maybe (M.Map Int Type)
mostGeneralUnifier (TVar i) b = Just $ M.singleton i b
mostGeneralUnifier a (TVar i) = Just $ M.singleton i a

mostGeneralUnifier TNumber TNumber = Just M.empty
mostGeneralUnifier TBoolean TBoolean = Just M.empty

mostGeneralUnifier (a :-> b) (c :-> d) = do
    s1 <- mostGeneralUnifier a c
    liftM2 mappend (mostGeneralUnifier (substitute s1 b) (substitute s1 d)) $
        Just s1

mostGeneralUnifier _ _ = Nothing

substitute :: M.Map Int Type -> Type -> Type
substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
substitute subs (a :-> b) = (substitute subs a) :-> (substitute subs b)
substitute _ t = t

typeTree :: Cofree AST () -> Maybe (Cofree AST Type)
typeTree c =
    let result = attribute c
        (r :< _) = result
        maybeSubs = solveConstraints . constraints $ snd r
    in  fmap (\subs -> fmap (substitute subs . fst) result) maybeSubs

