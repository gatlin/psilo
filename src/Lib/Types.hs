{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib.Types
where

-- |
-- Inspiration for this code came from
--   https://brianmckenna.org/blog/type_annotation_cofree
--
-- It has been modified for a slightly different type and more flexible type
-- grammar.

import Control.Monad (forM_, forM, foldM, liftM2, zipWithM, mapAndUnzipM)
import Control.Monad.Free
import Control.Monad.State hiding (sequence)
import Data.List (nub, union, intersperse)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid

import Control.Comonad
import Control.Comonad.Cofree
import Data.Foldable (Foldable, fold)
import qualified Data.Map as M

import Lib.Syntax

-- * Type term language

-- | The type language
-- Type signatures are equivalent to intuitionistic logic assertions, with the
-- arrow representing implication. Type checking and type inference can be done
-- by unification if one takes function arrows to be simply a kind of predicate
-- symbol.
--
-- The approach taken here is to treat type signatures as logical propositions
-- in a datalog-esque language. In lieu of predicates the type term language is
-- more general: there are concrete ground values, variables, and sequences of
-- other type expressions.
--
-- Thus a type signature like this
--
--     a -> (b -> c) -> c
--
-- could be represented in a prefix-notation like so:
--
--     TList [ TSym "->", TVar 1, TList [ TSym "->", TVar 2, TVar 3 ], TVar 3 ]
--
-- This affords us some flexibility: type classes, for instance, amount to extra
-- predicates, and their conditions can be easily encoded in this language as
-- well.
data Type
    = TVar Int
    | TSym String
    | TList [Type]
    deriving (Eq)

instance Show Type where
    show (TVar i) = "t" ++ (show i)
    show (TSym k) = k
    show (TList ts) = concat $ intersperse " " $ fmap show ts

-- * Type inference

-- | TODO there are more kinds of constraint than just this
data Constraint = EqualityConstraint Type Type
    deriving (Show)

-- | The result of inferring a type is a set of constraints and a set of
-- assumptions about the remaining type variables.
data TypeResult = TypeResult
    { constraints :: [Constraint]
    , assumptions :: M.Map String [Type]
    }

deriving instance Show TypeResult

instance Monoid TypeResult where
    mempty = TypeResult {
        constraints = mempty,
        assumptions = mempty }

    mappend a b = TypeResult {
        constraints = constraints a `mappend` constraints b,
        assumptions = assumptions a `mappend` assumptions b }

-- | The state of our type inference engine
data TypeState t m = TypeState
    { varId :: Int
    , memo :: M.Map t m
    }

defaultTypeState = TypeState {
    varId = 0,
    memo = M.empty
    }

type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)
type Untyped = Cofree CoreAst ()
type Attributed = Cofree CoreAst (Type, TypeResult)
type Typed = Cofree CoreAst Type

freshVarId :: State (TypeState t m) Type
freshVarId = do
    v <- gets varId
    modify $ \s -> s { varId = succ v }
    return $ TVar v

memoizedTC :: Ord c => (c -> TypeCheck c) -> c -> TypeCheck c
memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c where
    memoize = do
        r <- f c
        modify $ \s -> s { memo = M.insert c r $ memo s }
        return r

co :: Functor f => Free f t -> Cofree f ()
co (Free f) = () :< fmap co f

-- | Walk an expression tree and generate type constraints
attribute
    :: TypeState Untyped (Type, TypeResult)
    -> Untyped
    -> Attributed
attribute ts c =
    let initial = TypeState { memo = M.empty, varId = 0 }
    in  evalState (sequence $ extend (memoizedTC generateConstraints) c) ts

-- | Generate the constraints for the type inference algorithm
generateConstraints :: Cofree CoreAst () -> TypeCheck Untyped
generateConstraints (() :< IntC _) = return (TSym "Int", mempty)
generateConstraints (() :< DoubleC _) = return (TSym "Float", mempty)
generateConstraints (() :< BoolC _) = return (TSym "Boolean", mempty)

generateConstraints (() :< IdC i) = do
    var <- freshVarId
    return (var, TypeResult {
                   constraints = [],
                   assumptions = M.singleton i [var]
                   })

generateConstraints (() :< ClosC argSyms body) = do
    br <- memoizedTC generateConstraints body
    (vars, tr) <- foldM
        (\(vars, TypeResult cs as) arg -> do
                var <- freshVarId
                let cs' = maybe [] (map $ EqualityConstraint var)
                        (M.lookup arg as)
                    as' = M.delete arg as
                return (vars <> [var], TypeResult {
                               constraints = cs' <> cs,
                               assumptions = as'
                               }))
        ([], TypeResult [] (assumptions (snd br)))
        argSyms
    return (TList ([TSym "->"] <> vars <> [fst br]), TypeResult {
                   constraints = constraints (snd br) <> constraints tr,
                   assumptions = assumptions tr
                   })

generateConstraints (() :< AppC op erands) = do
    var <- freshVarId
    op' <- memoizedTC generateConstraints op
    (tys, results)  <- mapAndUnzipM (memoizedTC generateConstraints) erands
    let results' = foldl (<>) mempty results
    return (var, (snd op') <> results' <> TypeResult {
                   constraints = [EqualityConstraint (fst op') (
                                 TList $ [TSym "->"] <> tys <> [var])],
                   assumptions = mempty
                   })

-- | Get the concrete 'Type' value for any non-constants from a mapping
substitute :: M.Map Int Type -> Type -> Type
substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
substitute subs (TList ts) = TList $ map (substitute subs) ts
substitute _ t = t

-- | Attempt a mapping from type variables to types given a list of constraints
solveConstraints :: [Constraint] -> Maybe (M.Map Int Type)
solveConstraints =
    foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty
    where solve maybeSubs (EqualityConstraint a b) = do
              subs <- maybeSubs
              unify (substitute subs a) (substitute subs b)

-- | Simple unification for 'Type' values.
unify :: Type -> Type -> Maybe (M.Map Int Type)
unify (TVar i) b = Just $ M.singleton i b
unify a (TVar i) = Just $ M.singleton i a

unify (TSym a) (TSym b)
    | a == b = Just M.empty
    | otherwise = Nothing

unify (TList (a:as)) (TList (b:bs)) = do
    unified_heads <- unify a b
    liftM2 mappend (unify
                       (substitute unified_heads (TList as))
                       (substitute unified_heads (TList bs))) $
        Just unified_heads

unify _ _ = Nothing

-- | Gives a local type inference for a 'CoreExpr'
inferType :: CoreExpr () -> Maybe Typed
inferType expr =
    let expr' = co expr
        result = attribute defaultTypeState expr'
        (r :< _) = result
        maybeSubs = solveConstraints . constraints $ snd r
    in  fmap (\subs -> fmap (substitute subs . fst) result) maybeSubs
