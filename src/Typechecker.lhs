---
title: "Typechecker"
...

This code is lovingly stolen from

    http://brianmckenna.org/blog/type_annotation_cofree

Every value in psilo has a *type*. A type is a set of possible values. A value
may be a member of more than one type but in any given computation one must be
able to arrive at a single result before proceeding. By checking the types of
function arguments and their operands one can catch programming mistakes before
the program is executed.

A *type system* is the set of rules for how to define types and how they relate
to one another.

psilo's type system is going to be its strongsuit. For now we implement a very
basic type system which can be expanded upon later.

Imports and Language Extensions
---

> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE StandaloneDeriving #-}

> module Typechecker where
>
> import Prelude hiding (sequence)
>
> import Control.Comonad
> import Control.Comonad.Cofree
> import Control.Monad.Free
> import Control.Monad.State hiding (sequence)
> import Data.Foldable (Foldable, fold)
> import Data.Maybe (fromMaybe)
> import Data.Monoid
> import Data.Traversable (Traversable, sequence)
> import qualified Data.Map as M
>
> import Syntax

Our type language
---

> data Type
>     = TLambda [Type] Type
>     | TVar Int
>     | TNumber
>     | TBoolean
>     | TVoid
>
> deriving instance Show Type

Constraints
---

A type constraint is a means by which to narrow down the possible types for a
given value.

For now, there's only one kind of type constraint: the *equality constraint*.
An equality constraint means that two values which correspond - say, an
argument symbol and the value passed in - must have equal types.

There are other kinds of type constraints. This code is designed to be easy to
extend when the time comes.

> data Constraint = EqualityConstraint Type Type
> deriving instance Show Constraint

Results
---

A `TypeResult` is the result of each step of the algorithm. It consists of
*constraints* deduced by the syntax tree and *assumptions* which will be used
in future deductions.

> data TypeResult = TypeResult
>     { constraints :: [Constraint]
>     , assumptions :: M.Map String [Type]
>     }
>
> deriving instance Show TypeResult
>
> instance Monoid TypeResult where
>     mempty = TypeResult { constraints = mempty
>                         , assumptions = mempty }
>     mappend a b = TypeResult
>         { constraints = constraints a `mappend` constraints b
>         , assumptions = assumptions a `mappend` assumptions b
>         }

Stateful data to manage during algorithm execution

> data TypeState t m = TypeState
>     { varId :: Int
>     , memo  :: M.Map t m
>     }

Each node of our AST will return a type and a type result

> type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

Retrieve the next fresh variable id

> freshVarId :: State (TypeState t m) Type
> freshVarId = do
>     v <- gets varId
>     modify $ \s -> s { varId = succ v }
>     return $ TVar v

We want to memoize the result of each step to avoid repeat inference

> memoizedTC :: Ord c
>            => (c -> TypeCheck c)
>            -> c
>            -> TypeCheck c
> memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c where
>     memoize = do
>         r <- f c
>         modify $ \s -> s { memo = M.insert c r $ memo s }
>         return r

Convert the AST from its free monad rep to a cofree comonad

> --cofreeMu :: Functor f => Free f -> Cofree f ()
> cofreeMu (Free f) = () :< fmap cofreeMu f

Run our typechecking algorithm down the tree, generating type attrs.

> attribute :: Cofree AST () -> Cofree AST (Type, TypeResult)
> attribute c =
>     let initial = TypeState { memo = M.empty, varId = 0 }
>     in  evalState (sequence $ extend (memoizedTC generateConstraints) c)
>                   initial

The core of the algorithm. Generates type constraints.

> generateConstraints :: Cofree AST () -> TypeCheck (Cofree AST ())

> generateConstraints (() :< AInteger _) = return (TNumber, mempty)
> generateConstraints (() :< ABoolean _) = return (TBoolean, mempty)

> generateConstraints (() :< ASymbol s) = do
>     var <- freshVarId
>     return (var, TypeResult
>         { constraints = []
>         , assumptions = M.singleton s [var]
>         })

> generateConstraints (() :< ALambda args b) = do
>     argIds <- forM args $ \arg -> do
>         var <- freshVarId
>         return (arg, var)
>     br  <- memoizedTC generateConstraints b
>     let cs = forM argIds $ \(arg,var) -> do
>             return $ maybe [] (map $ EqualityConstraint var)
>                      (M.lookup arg . assumptions $ snd br)
>         as = forM argIds $ \(arg,_) -> do
>             return $ M.delete arg . assumptions $ snd br
>     return (TLambda (map snd argIds) (fst br), TypeResult
>         { constraints = constraints (snd br) `mappend` (mconcat (mconcat cs))
>         , assumptions = mconcat $ mconcat as
>         })

> generateConstraints (() :< AApply a b) = do
>     var <- freshVarId
>     ar  <- memoizedTC generateConstraints a
>     br  <- mapM (memoizedTC generateConstraints) b
>     return (var, snd ar `mappend` (mconcat (map snd br)) `mappend` TypeResult
>         { constraints = [EqualityConstraint (fst ar) $ TLambda (map fst br) var]
>         , assumptions = mempty
>         })

> generateConstraints (() :< ADefine sym val) = do
>     valType <- memoizedTC generateConstraints val
>     return (fst valType, TypeResult
>         { constraints = constraints (snd valType)
>         , assumptions = assumptions (snd valType)
>         })

Generate a type for the AST by solving all the generated constraints

> solveConstraints :: [Constraint] -> Maybe (M.Map Int Type)
> solveConstraints =
>     foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty where
>         solve maybeSubs (EqualityConstraint a b) = do
>             subs <- maybeSubs
>             mostGeneralUnifier (substitute subs a) (substitute subs b)

Given two types, get a map of substitutions if the types unify

> mostGeneralUnifier :: Type -> Type -> Maybe (M.Map Int Type)
> mostGeneralUnifier (TVar i) b = Just $ M.singleton i b
> mostGeneralUnifier a (TVar i) = Just $ M.singleton i a
>
> mostGeneralUnifier TNumber TNumber = Just M.empty
> mostGeneralUnifier TBoolean TBoolean = Just M.empty
>
> mostGeneralUnifier (TLambda as b) (TLambda cs d) = do
>     s1s <- forM (zip as cs) $ \(a, c) -> mostGeneralUnifier a c
>     mconcat $ map (\s1 -> liftM2 mappend (mostGeneralUnifier (substitute s1 b)
>                                        (substitute s1 d)) $ Just s1) s1s
>
> mostGeneralUnifier _ _ = Nothing

> substitute :: M.Map Int Type -> Type -> Type
> substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
> substitute subs (TLambda as b) = TLambda (map (substitute subs) as)
>                                          (substitute subs b)
> substitute _ t = t

> typeTree :: Cofree AST () -> Maybe (Cofree AST Type)
> typeTree c =
>     let result = attribute c
>         (r :< _) = result
>         maybeSubs = solveConstraints . constraints $ snd r
>     in  fmap (\subs -> fmap (substitute subs . fst) result) maybeSubs

