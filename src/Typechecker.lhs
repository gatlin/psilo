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

State
---

During the execution of the algorithm, we want to keep track of two things:

1. Type inferences we have already figured out;
2. Fresh variable IDs.

We don't want to repeat ourselves, especially in a large program. Values which
are re-used a lot should only have their types inferred once. Hence, we
*memoize* results and keep track of these memos.

Similarly, the types of function arguments are not initially known and hence
are variable. Each time we encounter a new symbol we want to create a new type
variable; later on we will try to substitute a concrete type. To this end we
will also keep track of *type variable IDs*.

Stateful data to manage during algorithm execution

> data TypeState t m = TypeState
>     { varId :: Int
>     , memo  :: M.Map t m
>     }

This function will generate a new ID (a monotonically increasing integer) and
store it.

> freshVarId :: State (TypeState t m) Type
> freshVarId = do
>     v <- gets varId
>     modify $ \s -> s { varId = succ v }
>     return $ TVar v

The type of our `State`-based monad is getting a bit unwieldly so we'll use the
following alias.

> type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

We want to memoize the result of each step to avoid repeat inference. If we
have not seen the value before, we will check it and store it. Other wise we
will return what is already known.

> memoizedTC :: Ord c
>            => (c -> TypeCheck c)
>            -> c
>            -> TypeCheck c
> memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c where
>     memoize = do
>         r <- f c
>         modify $ \s -> s { memo = M.insert c r $ memo s }
>         return r

Representing the AST
---

An `Expr` value - dealt with in the Parser and Evaluator - is a free monad
based on an `AST` value. The free monad representation allows for case-wise
translation of a functor into some other value. In essence, it lets you break
down a tree.

The cofree comonad, however, allows you to annotate a tree recursively. It is a
product type: the first element is a value of the stream in focus, and the
second element is a functor which can be used to focus other values.

The way we use the cofree comonad is to store our annotation of each node as
the first value and its parents in the second value. Initially the untyped tree
will simply use `()` as its annotation.

> cofreeMu :: Functor f => Free f t -> Cofree f ()
> cofreeMu (Free f) = () :< fmap cofreeMu f

Following from this, we can write a function to perform the traversal starting
with an empty memoization map and an initial type variable ID of `0`.

> attribute :: Cofree AST () -> Cofree AST (Type, TypeResult)
> attribute c =
>     let initial = TypeState { memo = M.empty, varId = 0 }
>     in  evalState (sequence $ extend (memoizedTC generateConstraints) c)
>                   initial

Constraint generation
---

We can now represent, traverse, and annotate a tree recursively, fluidly
converting it from its free monad representation.

Our algorithm is a bottom-up constraint satisfaction algorithm. Starting with
the leaves, each value might constrain the types of functions in which they are
bound or yield assumptions about the mappings from symbols to types. When the
algorithm is complete either each symbol will map to a concrete type and it
will all match, or there will be a type error.

> generateConstraints :: Cofree AST () -> TypeCheck (Cofree AST ())

Primitive literals are the easiest.

> generateConstraints (() :< AInteger _) = return (TNumber, mempty)
> generateConstraints (() :< ABoolean _) = return (TBoolean, mempty)

Symbols on their own are each the single value of their own distinct types,
until they are bound by constraints to a concrete type.

> generateConstraints (() :< ASymbol s) = do
>     var <- freshVarId
>     return (var, TypeResult
>         { constraints = []
>         , assumptions = M.singleton s [var]
>         })

Function abstraction takes each of its bound variables out of its body's
assumption map and turns them into input constraints.

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

Function application generates constraints and fresh type variables for each
argument and the return value, along with a constraint that the function takes
a number of arguments of certain types and returns the return type.

> generateConstraints (() :< AApply a b) = do
>     var <- freshVarId
>     ar  <- memoizedTC generateConstraints a
>     br  <- mapM (memoizedTC generateConstraints) b
>     return (var, snd ar `mappend` (mconcat (map snd br)) `mappend` TypeResult
>         { constraints = [EqualityConstraint (fst ar) $ TLambda (map fst br) var]
>         , assumptions = mempty
>         })

Definitions are a bit obtuse: they are declarative mutations to the static
environment and thus do not compose or return anything useful. As a result,
they are simply the type of whatever value stored in them.

> generateConstraints (() :< ADefine sym val) = do
>     valType <- memoizedTC generateConstraints val
>     return (fst valType, TypeResult
>         { constraints = constraints (snd valType)
>         , assumptions = assumptions (snd valType)
>         })

Constraint solving
---

We now have our tree, a means of traversing it, and a set of constraints and
type assumptions. We now need to solve the generated constraints and type the
whole program.

Generate a type for the AST by solving all the generated constraints:

> solveConstraints :: [Constraint] -> Maybe (M.Map Int Type)
> solveConstraints =
>     foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty where
>         solve maybeSubs (EqualityConstraint a b) = do
>             subs <- maybeSubs
>             mostGeneralUnifier (substitute subs a) (substitute subs b)

Given two types, get a map of substitutions if the types unify:

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

Actually substitute the mappings in the type, yielding a more specific type.

> substitute :: M.Map Int Type -> Type -> Type
> substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
> substitute subs (TLambda as b) = TLambda (map (substitute subs) as)
>                                          (substitute subs b)
> substitute _ t = t

Putting it all together
---

Given a cofree comonad representation of an AST where each node is annotated
with `()`, yield a new tree where each node is annotate with its type.

> typeTree :: Cofree AST () -> Maybe (Cofree AST Type)
> typeTree c =
>     let result = attribute c
>         (r :< _) = result
>         maybeSubs = solveConstraints . constraints $ snd r
>     in  fmap (\subs -> fmap (substitute subs . fst) result) maybeSubs

