{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Data.List (nub, union, intersperse, sort)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid

import Control.Comonad
import Control.Comonad.Cofree
import Data.Foldable (Foldable, fold)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

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

data Scheme = Scheme [Int] Type
    deriving (Eq, Show)

type Frame = M.Map Int Type

-- | An expression which may contain free type variables
class Typing t where
    ftv :: t -> S.Set Int
    substitute :: Frame -> t -> t

instance Typing Type where
    ftv (TVar n) = S.singleton n
    ftv (TSym _) = S.empty
    ftv (TList ts) = foldl S.union S.empty $ map ftv ts

    -- | Get the concrete 'Type' value for any non-constants from a mapping
    substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
    substitute subs (TList ts) = TList $ map (substitute subs) ts
    substitute _ t = t

instance Typing Scheme where
    ftv (Scheme vars t) = (ftv t) `S.difference` (S.fromList vars)

    substitute frame (Scheme vars t) = Scheme vars $
        substitute (foldr M.delete frame vars) t

-- * Type inference

-- | TODO there are more kinds of constraint than just this
data Constraint
    = EqualityConstraint Type Type
    | ExplicitConstraint Type Scheme
    | ImplicitConstraint (S.Set Int) Type Type
    deriving (Show, Eq)

instance Ord Constraint where
    (EqualityConstraint _ _) <= (ExplicitConstraint _ _) = True
    _ <= _ = False

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
        constraints = sort $ constraints a `mappend` constraints b,
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
    , builtins :: M.Map Symbol (Type, TypeResult)
    , definitions :: M.Map Symbol Untyped
    }

defaultTypeState = TypeState {
    varId = 0,
    memo = M.empty,
    builtins = M.empty,
    definitions = M.empty
    }

type TypeCheck t = State (TypeState t (Type, TypeResult))

freshVarId :: State (TypeState t m) Type
freshVarId = do
    v <- gets varId
    modify $ \s -> s { varId = succ v }
    return $ TVar v

memoizedTC
    :: Ord c
    => (c -> TypeCheck c (Type, TypeResult))
    -> c
    -> TypeCheck c (Type, TypeResult)
memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c where
    memoize = do
        r <- f c
        modify $ \s -> s { memo = M.insert c r $ memo s }
        return r

-- | Walk an expression tree and generate type constraints
attribute
    :: TypeState Untyped (Type, TypeResult)
    -> [Untyped]
    -> TypeCheck Untyped [Attributed]
attribute ts cs = forM cs $ \c ->
    sequence $ extend (memoizedTC generateConstraints) c

-- | Generate the constraints for the type inference algorithm
generateConstraints :: Untyped -> TypeCheck Untyped (Type, TypeResult)
generateConstraints (() :< IntC _) = return (TSym "Int", mempty)
generateConstraints (() :< DoubleC _) = return (TSym "Float", mempty)
generateConstraints (() :< BoolC _) = return (TSym "Boolean", mempty)

generateConstraints (() :< IdC i) = do
    bs <- gets builtins
    ds <- gets definitions
    (var, tr) <- case M.lookup i bs of
        Just (ty,tr)-> return (ty, tr)
        Nothing -> case M.lookup i ds of
            Nothing -> freshVarId >>= return . flip (,) mempty
            Just untyped -> do
                ur <- memoizedTC generateConstraints untyped
                modify $ \st -> st {
                    builtins = M.insert i ur bs }
                return ur
    return (var, tr <> TypeResult {
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
    let cs = [ (EqualityConstraint (fst op') $
                TList ([TSym "->"] ++ tys ++ [var])) ]
    return (var, (snd op') <> results' <> TypeResult {
                   constraints = cs,
                   assumptions = mempty
                   })

generateConstraints (() :< IfC c t e) = do
    var <- freshVarId
    cr <- memoizedTC generateConstraints c
    tr <- memoizedTC generateConstraints t
    er <- memoizedTC generateConstraints e
    return (var, snd cr <> snd tr <> snd er <> TypeResult {
                   constraints = [ EqualityConstraint (fst tr) (fst er),
                                   EqualityConstraint var (fst tr),
                                   EqualityConstraint (fst cr) (TSym "Boolean")
                                 ],
                   assumptions = mempty
                   })

generateConstraints (() :< DefC sym expr) = do
    var <- freshVarId
    (t, tr) <- memoizedTC generateConstraints expr
    let s = ftv t -- FIXME this is not the correct construction of this set
    return (var, tr <> TypeResult {
                   constraints = [ ImplicitConstraint s var t ],
                   assumptions = mempty
                   })

instantiate :: Scheme -> TypeCheck Untyped Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> freshVarId) vars
    let s = M.fromList (zip vars nvars)
    return $ substitute s t

generalize :: S.Set Int -> Type -> Scheme
generalize vars t = Scheme vars' t
    where vars' = S.toList $ (ftv t) `S.difference` vars

activeVars :: Constraint -> S.Set Int
activeVars (EqualityConstraint t1 t2) = (ftv t1) `S.union` (ftv t2)
activeVars (ExplicitConstraint t s) = (ftv t) `S.union` (ftv s)
activeVars (ImplicitConstraint m t1 t2) = (ftv t1) `S.union` (m `S.intersection` (ftv t2))

-- | Attempt a mapping from type variables to types given a list of constraints

solveConstraints :: [Constraint] -> TypeCheck Untyped (Maybe Frame)
solveConstraints cs = go (Just mempty) cs where
    go result [] = return result
    go Nothing _ = return Nothing
    go (Just subs) ((EqualityConstraint a b) : cs) = do
        let subs' = unify (substitute subs a) (substitute subs b) $ Just subs
        go subs' cs

    go (Just subs) ((ExplicitConstraint a b) : cs) = do
        c <- instantiate b
        let subs' = unify (substitute subs a) (substitute subs c) $ Just subs
        go subs' cs

    go (Just subs) ((ImplicitConstraint vars a b) : cs) = do
        let avs = foldl S.union S.empty $ map activeVars cs
        if (((ftv b) `S.difference` vars) `S.intersection` avs) == S.empty
           then do
             c <- instantiate $ generalize vars b
             let subs' = unify (substitute subs a) (substitute subs c) $
                         Just subs
             go subs' cs
           else return $ Just subs

-- | Simple unification for 'Type' values
unify :: Type -> Type -> Maybe Frame -> Maybe Frame
unify t1 t2 (Just frame)
    | t1 == t2 = Just frame
    | otherwise = go t1 t2 where

          go (TVar _) t2 = unify_var t1 t2 $ Just frame
          go t1 (TVar _) = unify_var t2 t1 $ Just frame

          go (TList (a:as)) (TList (b:bs)) =
              unify (TList as) (TList bs)  $
                unify (substitute frame a)
                      (substitute frame b) $
                Just frame

          go _ _ = Nothing

unify _ _ Nothing = Nothing

unify_var :: Type -> Type -> Maybe Frame -> Maybe Frame
unify_var var@(TVar n) val (Just frame)
    | var == val = Just frame
    | otherwise = case M.lookup n frame of
                      Nothing -> if occurs_check var val frame
                                    then Just $ M.insert n val frame
                                    else Nothing
                      Just val' -> unify val' val $ Just frame

unify_var _ _ Nothing = Nothing

occurs_check :: Type -> Type -> Frame -> Bool
occurs_check var@(TVar x) val frame = go (substitute frame val) where
    go (TSym _) = True
    go (TVar y)
        | var == val = True
        | otherwise = case M.lookup y frame of
                          Nothing -> True
                          Just v' -> go v'
    go (TList (x:xs)) = and [ go x, go (TList xs) ]
    go (TList []) = True

int_type = TVar 0
float_type = TSym "Float"
mul_type = TList [ TSym "->", int_type, int_type, int_type ]
mul_res = TypeResult [ ExplicitConstraint (TVar 0) $
                       Scheme [0] (TList [ TSym "Num", TVar 0 ] ) ]
          mempty

infer :: Attributed -> TypeCheck Untyped (Maybe Frame)
infer attr = solveConstraints $ constraints $ snd $ extract attr

test :: IO ()
test = do
    defns <- parse_multi "(defun square (x) (* x x)) (def four (square 2))"
    --defns <- parse_multi "(defun wut (x) (if x 3 2))"
    putStrLn . show $ defns
    putStrLn "***"
    let tyState = defaultTypeState {
            builtins = M.fromList [("*", (mul_type, mul_res))],
            definitions = M.fromList (
                fmap (\(Free (DefC sym expr)) -> (sym, co expr)) defns),
            varId = 1 }

    let (defns', tyState') = runState (attribute tyState $ fmap co defns) tyState
    forM_ defns' $ putStrLn . show
    putStrLn "***"
    let (inferred, ts) = runState (fmap (foldl (<>) mempty) $
                   sequence $
                   fmap infer defns') tyState'
    forM_ inferred $ putStrLn . show
    putStrLn "***"
    forM_ defns' $ \d -> do
        putStrLn . show $ fmap
            (\subs -> fmap (substitute subs . fst) d)
            inferred
    putStrLn "***"
    putStrLn . show . memo $ ts
    putStrLn "***"
    putStrLn . show . builtins $ ts
