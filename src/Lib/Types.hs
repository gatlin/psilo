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
-- I also liberally plagiarized this page:
--   http://dev.stephendiehl.com/fun/006_hindley_milner.html#generalization-and-instantiation
--
-- It has been modified for a slightly different type and more flexible type
-- grammar.

import Control.Monad (forM_, forM, foldM, liftM2, zipWithM, mapAndUnzipM, when)
import Control.Monad.Free
import Control.Monad.State hiding (sequence)
import Control.Monad.IO.Class
import Data.List (nub, union, intercalate, sort)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid

import Control.Comonad
import Control.Comonad.Cofree
import Data.Foldable (Foldable, fold)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.Set (Set)
import qualified Data.Set as S

import Lib.Syntax
import Lib.Parser (parse_expr, parse_multi)
import qualified Data.Text as T

-- | The type language. Perhaps this will change but for flexibility types are
-- composed of variables, constant symbols, or sequences of the two.
data Type
    = TVar Int
    | TSym Symbol
    | TList [Type]
    deriving (Ord, Eq)

instance Show Type where
    show (TVar n) = "t" ++ (show n)
    show (TSym sym) = sym
    show (TList ts) = parens ts' where
        parens inside = "(" ++ inside ++ ")"
        ts' = intercalate " " $
              map show ts

lambdaType :: [ Type ] -> Type
lambdaType tys = TList $ (TSym "->") : tys

-- | A type scheme is a polymorphic type with quantified type variables. They
-- allow polymorphic types to instantiated in different ways depending on
-- context.
data Scheme = Forall [Int] Type
    deriving (Eq, Ord, Show)

-- | A mapping from type variables to concrete types. The goal of type inference
-- is to construct a frame for a given program that is consistent and sound.
newtype Frame = Frame {
    bindings :: Map Int Type
    } deriving (Eq, Show)

instance Monoid Frame where
    mempty = Frame mempty
    (Frame f1) `mappend` (Frame f2) = Frame $
        M.map (substitute (Frame f1)) f2 `M.union` f1

frameLookup :: Frame -> Int -> Maybe Type
frameLookup (Frame frame) i = M.lookup i frame

frameInsert :: Frame -> Int -> Type -> Frame
frameInsert (Frame frame) i ty = Frame $ M.insert i ty frame

frameDelete :: Frame -> Int -> Frame
frameDelete (Frame frame) i = Frame $ M.delete i frame

-- | A class for things which may contain type variables or be substituted for
-- types given a 'Frame'.
class Typing t where
    -- | free type variables
    ftv :: t -> IntSet

    -- | perform a substitution
    substitute :: Frame -> t -> t

instance Typing Type where
    ftv (TVar n) = I.singleton n
    ftv (TSym s) = mempty
    ftv (TList ts) = foldl (<>) mempty $ fmap ftv ts

    substitute frame v@(TVar n) = maybe v (substitute frame) $
        frameLookup frame n

    substitute frame (TList ts) = TList $ map (substitute frame) ts
    substitute _ t = t

instance Typing Scheme where
    ftv (Forall vars t) = (ftv t) `I.difference` (I.fromList vars)

    substitute (Frame frame) (Forall vars t) = Forall vars $
        substitute (Frame $ foldr M.delete frame vars) t

instance Typing a => Typing [a] where
    ftv = foldr (I.union . ftv) I.empty
    substitute = map . substitute

-- | If two types are constrained then there must be a valid unification for
-- them. Inference fails unless all constraints are satisfied.
data Constraint
    = Type := Type -- ^ equality constraint
    | Type :~ Scheme -- ^ instance constraint
    deriving (Show, Eq, Ord)

data TypeResult = TypeResult
    { constraints :: [ Constraint ]
    , assumptions :: Map String [ Type ]
    } deriving (Show)

instance Monoid TypeResult where
    mempty = TypeResult [] mempty
    mappend a b = TypeResult {
        constraints = constraints a <> constraints b,
        assumptions = assumptions a <> assumptions b
        }

-- | An untyped representation of an expression.
type Untyped = Cofree CoreAst ()

-- Convert a 'CoreExpr' to an 'Untyped' expression.
untyped :: Functor f => Free f t -> Cofree f ()
untyped (Free f) = () :< fmap untyped f

-- | Top-level definitions are parametrically polymorphic.
newtype TypeEnv = TypeEnv (Map Symbol Scheme)
    deriving (Show)

envInsert :: TypeEnv -> Symbol -> Scheme -> TypeEnv
envInsert (TypeEnv te) name scheme = TypeEnv $ M.insert name scheme te

instance Monoid TypeEnv where
    mempty = TypeEnv mempty
    mappend (TypeEnv a) (TypeEnv b) = TypeEnv $ a `M.union` b

instance Typing TypeEnv where
    ftv (TypeEnv te) = ftv $ M.elems te

    substitute s (TypeEnv te) = TypeEnv $ M.map (substitute s) te

-- | During inference we will want: a source of fresh, unique type variables; a
-- map to memoize inference and avoid recomputing the same expressions multiple
-- times; and a type environment for top level definitions that need to be
-- instantiated.
data TypeState = TypeState
    { varId :: Int
    , memo :: Map Untyped (Type, TypeResult)
    , typeEnv :: TypeEnv
    , definitions :: Map Symbol Untyped
    }

newTypeState :: TypeState
newTypeState = TypeState 0 mempty mempty mempty

-- | A simple type checking monad.
type TypeCheck = StateT TypeState IO

-- | This function returns a fresh supply of type variables.
freshVarId :: TypeCheck Type
freshVarId = do
    v <- gets varId
    modify $ \s -> s { varId = succ v }
    return $ TVar v

-- | The actual memoization function for the 'TypeCheck' monad.
memoizedTC
    :: (Untyped -> TypeCheck (Type, TypeResult))
    -> Untyped
    -> TypeCheck (Type, TypeResult)
memoizedTC f c@(() :< c') = gets memo >>= maybe memoize return . M.lookup c where
    memoize = case c' of
        IdC _ -> f c
        _ -> do
            r <- f c
            modify $ \s -> s { memo = M.insert c r $ memo s }
            return r

-- | Generate type constraints based on Damas-Hindley-Milner type rules to be
-- solved later.
generateConstraints :: Untyped -> TypeCheck (Type, TypeResult)

generateConstraints (() :< IntC _) = return (TSym "Int", mempty)
generateConstraints (() :< DoubleC _) = return (TSym "Float", mempty)
generateConstraints (() :< BoolC _) = return (TSym "Boolean", mempty)

generateConstraints (() :< IdC sym) = do
    defns <- gets definitions
    (TypeEnv te) <- gets typeEnv
    case M.lookup sym defns of
        Just u -> do
            (ty, tr) <- generateConstraints u
            let sc = closeOver ty
            return (ty, tr <> TypeResult {
                           constraints = [ ty :~ sc ],
                           assumptions = mempty })
        Nothing -> case M.lookup sym te of
            Just sc -> do
                var <- freshVarId
                return (var, TypeResult [ var :~ sc ] mempty)
            Nothing -> do
                var <- freshVarId
                return (var, TypeResult [] (M.singleton sym [var]))

generateConstraints (() :< ClosC argSyms body) = do
    br <- memoizedTC generateConstraints body
    (vars, tr) <- foldM
        (\(vars, TypeResult cs as) arg -> do
                var@(TVar n) <- freshVarId
                let cs' = maybe [] (map (var :=))
                          (M.lookup arg as)
                    as' = M.delete arg as
                return (vars <> [var], TypeResult {
                               constraints = cs' <> cs,
                               assumptions = as'
                               }))
        ([], TypeResult [] (assumptions $ snd br))
        argSyms
    return (lambdaType (vars <> [fst br]), TypeResult {
                   constraints = constraints (snd br) <> constraints tr,
                   assumptions = assumptions tr
                   })

generateConstraints (() :< AppC op erands) = do
    var <- freshVarId
    op' <- memoizedTC generateConstraints op
    (tys, erands') <- mapAndUnzipM (memoizedTC generateConstraints) erands
    let results = foldl (<>) mempty erands'
    let cs = [ (fst op') := (lambdaType (tys <> [var])) ]
    return (var, snd op' <> results <> TypeResult {
                   constraints = cs,
                   assumptions = mempty
                   })

generateConstraints (() :< IfC c t e) = do
    var <- freshVarId
    cr <- memoizedTC generateConstraints c
    tr <- memoizedTC generateConstraints t
    er <- memoizedTC generateConstraints e
    return (var, snd cr <> snd tr <> snd er <> TypeResult {
                   constraints = [ (fst tr) := (fst er)
                                 , var := (fst tr)
                                 , (fst cr) := (TSym "Boolean")
                                 ],
                   assumptions = mempty
                   })

-- | Instantiating a type scheme replaces all of its type variables with new
-- unique variables. Each time a polymorphic definition is used it is
-- instantiated differently.
instantiate :: Scheme -> TypeCheck Type
instantiate (Forall vars t) = do
    nvars <- mapM (\_ -> freshVarId) vars
    let f = Frame $ M.fromList (zip vars nvars)
    return $ substitute f t

-- | When a top level definition is inferred, its type is generalized into a
-- type scheme so that it may be instantiated multiple times later depending on
-- usage.
generalize :: TypeEnv -> Type -> Scheme
generalize te t = Forall as t
    where as = I.toList $ ftv t `I.difference` ftv te

-- | Type schemes don't need to have globally unique type variables.
normalize :: Scheme -> Scheme
normalize (Forall _ t) = Forall (map snd ord) (normtype t)
    where
        ord = zip (nub $ fv t) [1..]
        fv = I.toList . ftv

        normtype (TList ts) = TList $ map normtype ts
        normtype (TSym sym) = TSym sym
        normtype (TVar n) = case Prelude.lookup n ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"

-- | Generalize and normalize a type into a type scheme in one smooth motion.
closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

-- * Unification!

unify :: Type -> Type -> Maybe Frame -> Maybe Frame
unify t1 t2 (Just frame)
    | t1 == t2 = Just frame
    | otherwise = go t1 t2 where

          go (TVar _) t2 = unify_var t1 t2 frame
          go t1 (TVar _) = unify_var t2 t1 frame

          go (TList (a:as)) (TList (b:bs)) =
              unify (TList as) (TList bs) $
              unify (substitute frame a) (substitute frame b) $
              Just frame

          go (TSym a) (TSym b)
              | a == b = Just frame
              | otherwise = Nothing

          go _ _ = Nothing

unify _ _ Nothing = Nothing

unify_var :: Type -> Type -> Frame -> Maybe Frame
unify_var var@(TVar n) val frame
    | var == val = Just frame
    | otherwise = case frameLookup frame n of
                      Nothing -> if occurs_check var val frame
                                    then Just $ frameInsert frame n val
                                    else Nothing

                      Just val' -> unify val' val $ Just frame

unify_var _ _ _ = Nothing

occurs_check :: Type -> Type -> Frame -> Bool
occurs_check var@(TVar x) val frame = go (substitute frame val)
    where go (TSym _) = True
          go (TVar y)
              | var == val = True
              | otherwise = case frameLookup frame y of
                                Nothing -> True
                                Just v' -> go v'

          go (TList (x:xs)) = and [ go x, go (TList xs) ]
          go (TList []) = True

-- | The set of constraints is unified one by one until failure or a 'Frame' has
-- been constructed.
solveConstraints :: [Constraint] -> TypeCheck (Maybe Frame)
solveConstraints cs = go (Just mempty) cs where
    go result [] = return result
    go Nothing _ = return Nothing

    go f@(Just frame) ((a := b) : cs) = do
        let fr = unify (substitute frame a) (substitute frame b) $ Just frame
        go (fr <> f) cs

    go f@(Just frame) ((a :~ b) : cs) = do
        b' <- instantiate (substitute frame b)
        let fr = unify (substitute frame a) (substitute frame b') $ Just frame
        go (fr <> f) cs

modifyTypeEnv :: (TypeEnv -> TypeEnv) -> TypeCheck ()
modifyTypeEnv f = gets typeEnv >>= \te -> modify $ \st -> st { typeEnv = f te }

inferTop :: TypeEnv -> [(Symbol, Untyped)] -> IO (Maybe TypeEnv)
inferTop te [] = return $ Just te
inferTop te exprs = (flip evalStateT) ts $ do

    ccs <- forM exprs $ \(name, expr) -> do
        (ty, tr) <- memoizedTC generateConstraints expr
        modifyTypeEnv $ \te -> envInsert te name $ Forall [] ty
        return $ constraints tr

    let cs = concat ccs
    mFrame <- solveConstraints cs
    case mFrame of
        Nothing -> return Nothing
        Just frame -> do
            te <- gets typeEnv
            return . Just $ substitute frame te

    where ts = newTypeState {
              typeEnv = te,
              definitions = M.fromList exprs }

num_type = TSym "Int"
mul_type = generalize mempty $ lambdaType [num_type, num_type, num_type]
add_type = generalize mempty $ lambdaType [num_type, num_type, num_type]
te = TypeEnv $ M.fromList [("*", mul_type), ("+", add_type)]
test :: IO ()
test = do
    defns <- parse_multi $ T.concat
        [ "(defun times-1 (x) (* 1 x))"
        , "(def nine (square 3))"
        , "(defun id (z) z)"
        , "(defun square (y) (id (* y y)))"
        , "(def four (square 2))"
        , "(defun box (b) (\\ (f) (f b)))"
        ]
    let defns' = map (\(Free (DefC sym expr)) -> (sym, untyped expr)) defns
    putStrLn . show $ defns'
    putStrLn "***"
    results <- inferTop te defns'
    putStrLn . show $ results
