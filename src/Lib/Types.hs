{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleContexts #-}

module Lib.Types
where

-- |
-- Inspiration for this code came from
--   https://brianmckenna.org/blog/type_annotation_cofree
--
-- It has been modified for a slightly different type and more flexible type
-- grammar.

import Control.Monad (forM_, forM, foldM, liftM2, zipWithM, mapAndUnzipM, when)
import Control.Monad.Free
import Control.Monad.State hiding (sequence)
import Data.List (nub, union, intersperse)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid

import Control.Comonad
import Control.Comonad.Cofree
import Data.Foldable (Foldable, fold)
import qualified Data.Map.Lazy as M

import Lib.Syntax
import Lib.Parser (parse_expr, parse_multi)
import qualified Data.Text as T

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
    show (TList ts) =
        "(" ++ (concat $ intersperse " " $ fmap show ts) ++ ")"

-- * Type inference

-- | TODO there are more kinds of constraint than just this
data Constraint
    = EqualityConstraint Type Type
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

co :: Functor f => Free f t -> Cofree f ()
co (Free f) = () :< fmap co f

type Untyped = Cofree CoreAst ()
type Attributed = Cofree CoreAst (Type, TypeResult)
type Typed = Cofree CoreAst Type

-- | The state of our type inference engine
data TypeState t m = TypeState
    { varId :: Int
    , memo :: M.Map t m
    , builtins :: M.Map Symbol Type
    }

defaultTypeState = TypeState {
    varId = 0,
    memo = M.empty,
    builtins = M.empty
    }

type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

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

-- | Walk an expression tree and generate type constraints
attribute
    :: TypeState Untyped (Type, TypeResult)
    -> [Untyped]
    -> [Attributed]
attribute ts cs = evalState go ts where
    go = forM cs $ \c ->
        sequence $ extend (memoizedTC generateConstraints) c


-- | Generate the constraints for the type inference algorithm
generateConstraints :: Cofree CoreAst () -> TypeCheck Untyped
generateConstraints (() :< IntC _) = return (TSym "Int", mempty)
generateConstraints (() :< DoubleC _) = return (TSym "Float", mempty)
generateConstraints (() :< BoolC _) = return (TSym "Boolean", mempty)

generateConstraints (() :< IdC i) = do
--    var <- freshVarId
    bs <- gets builtins
    var <- case M.lookup i bs of
        Nothing -> freshVarId
        Just ty -> return ty
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
                   constraints = [EqualityConstraint (fst op')
                                 (TList $ [TSym "->"] <> tys <> [var])],
                   assumptions = mempty
                   })

generateConstraints (() :< IfC c t e) = do
    var <- freshVarId
    cr <- memoizedTC generateConstraints c
    tr <- memoizedTC generateConstraints t
    er <- memoizedTC generateConstraints e
    return (fst tr, snd cr <> snd tr <> snd er <> TypeResult {
                   constraints = [ EqualityConstraint (fst tr) (fst er),
                                   EqualityConstraint var (fst tr),
                                   EqualityConstraint (fst cr) (TSym "Boolean")
                                 ],
                   assumptions = mempty
                   })

generateConstraints (() :< DefC sym expr) = do
    (t, tr) <- memoizedTC generateConstraints expr
    return (TSym sym, tr <> TypeResult {
                   constraints = [ ],
                   assumptions = mempty
                   })

type Frame = M.Map Int Type

-- | Get the concrete 'Type' value for any non-constants from a mapping
substitute :: Frame -> Type -> Type
substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
substitute subs (TList ts) = TList $ map (substitute subs) ts
substitute _ t = t

-- | Attempt a mapping from type variables to types given a list of constraints
solveConstraints :: [Constraint] -> Maybe Frame
solveConstraints =
    foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty
    where solve maybeSubs (EqualityConstraint a b) = do
              subs <- maybeSubs
              unify (substitute subs a) (substitute subs b) maybeSubs

-- | Simple unification for 'Type' values
unify :: Type -> Type -> Maybe Frame -> Maybe Frame
unify t1 t2 frame
    | isNothing frame = Nothing
    | t1 == t2 = frame
    | otherwise = go t1 t2 where

          go (TVar _) t2 = unify_var t1 t2 frame
          go t1 (TVar _) = unify_var t2 t1 frame

          go (TList (a:as)) (TList (b:bs)) =
              unify (TList as) (TList bs)  $ unify a b frame

          go _ _ = Nothing

unify_var :: Type -> Type -> Maybe Frame -> Maybe Frame
unify_var var@(TVar n) val frame
    | isNothing frame = Nothing
    | var == val = frame
    | otherwise = case M.lookup n (fromJust frame) of
                      Nothing -> if occurs_check var val frame
                                    then fmap (M.insert n val) frame
                                    else Nothing
                      Just val' -> unify val' val frame

occurs_check :: Type -> Type -> Maybe Frame -> Bool
occurs_check var@(TVar x) val (Just frame) = go val where
    go (TSym _) = True
    go (TVar y)
        | var == val = True
        | otherwise = case M.lookup y frame of
                          Nothing -> True
                          Just v' -> go v'
    go (TList (x:xs)) = and [ go x, go (TList xs) ]
    go (TList []) = True

num_type = TList [ TSym "Num", TVar 0 ]
mul_type = TList [ TSym "->", num_type, num_type, num_type ]

test :: IO ()
test = do
    --defns <- parse_multi "(defun square (x) (* x x)) (def four (square 2))"
    defns <- parse_multi "(defun wut (x) (if x 3 2))"
    putStrLn . show $ defns
    putStrLn "***"
    let tyState = defaultTypeState {
            builtins = M.fromList [("*", mul_type)],
            varId = 1 }
    let defns' = attribute tyState $ fmap co defns
    forM_ defns' $ \a -> do
        let cs = constraints $ snd $ extract a
        putStrLn . show $ cs
        let solved = solveConstraints cs
        putStrLn $ "Frame: " ++ (show solved)
        let results = fmap (\subs -> fmap (substitute subs . fst)
                           a) solved
        putStrLn . show $ results
        putStrLn "---"
    {-
    forM_ defns $ \expr -> do
        putStrLn . show $ expr
        let expr' = co expr
        let attributed = attribute tyState expr'
        let cs = constraints $ snd $ extract attributed
        putStrLn . show $ cs
        let solved = solveConstraints cs
        putStrLn $ "Frame: " ++ (show solved)
        let results = fmap (\subs -> fmap (substitute subs . fst)
                               attributed) solved
        putStrLn . show $ results
        putStrLn "---"
-}
