---
title: "The psilo Virtual Machine"
...

The virtual machine consists of two things:

* a statically scoped mapping from symbols to store locations (the "environment"); and
* a persistent mapping from locations to values (the "store").

Thus a `Machine` is a monad composed of `ReaderT` and `StateT` monad
transformers.

The Reader monad permits function-local overwriting of the contained state
which is automatically rolled back -- precisely the behavior we want out of
our lexical environment.

The State monad, on the other hand, is persistent until the end of the
machine's execution and thus handles dynamic scope and state.

Please note that this is simply a reference implementation of the virtual
machine to start playing with psilo's grammar and other features; by no means
is this intended to be psilo's evaluator (yet), just an experimental test-bed.

Imports and language extensions
---

> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE AllowAmbiguousTypes #-}
>
> module Evaluator
>
> ( Machine(..)
> , Storage
> , Environment
> , Location
> , MachineState(..)
> , runMachine
> , newMachineState
> , eval
> , strict
> , evalWithContext
> )
>
> where
>
> import Control.Monad.Free
> import Prelude hiding (log)
> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Writer
> import Control.Monad.Free
> import Control.Monad.Trans
> import qualified Data.IntMap.Strict as IntMap
> import Data.Foldable (Foldable, fold)
> import Data.Traversable (Traversable, sequence)
> import Data.List (intersperse, nub, (\\))
> import Data.Monoid
> import Control.Applicative
>
> import Parser
> import Syntax

The Machine
---

Borrowing (stealing?) from Krishnamurthi's inimitable [Programming Languages:
Application and Interpretation][plai] the environment does not map symbols to
values but to *locations* in the store. The store, then, maps location to
values.

What is the result of evaluating an `Expr` with a `Machine`? There should be
some ultimate result type onto which we can map our `Expr`s.

> data Value
>     = VNil
>     | VInteger Integer
>     | VBoolean Bool
>     | VClosure { clArgs :: [Symbol]
>                , clBody :: (Expr ())
>                , clEnv  :: ClosureEnv }
>     | VThunk (Expr ()) ClosureEnv
>     deriving (Eq, Show)

To make the code more readable, an environment mapping `Symbol`s to `Expr ()`
values is called a `ClosureEnv`:

> type ClosureEnv = Environment Value

In our case, the values being stored are `Value`s. Our initial evaluation
strategy will be call-by-need.

> type Location = Int
> type Storage = IntMap.IntMap Value
> type Environment a = [(Symbol, a)]

> data MachineState = MachineState
>     { mStore :: Storage
>     , mEnv   :: Environment Location
>     , mLoc   :: Location
>     } deriving (Show)

> newMachineState = MachineState (IntMap.empty) [] 0

> newtype Machine a = M {
>     runM :: WriterT [String] (StateT MachineState IO) a
> } deriving (Monad, MonadIO, MonadState MachineState
>            , MonadWriter [String], Functor)

Haskell will soon consider `Monad`s which are not also `Applicative`s as
errors, but fortunately we can handle this trivially:

> instance Applicative Machine where
>     pure = return
>     (<*>) = ap

> runMachine :: MachineState -> Machine a -> IO ((a, [String]), MachineState)
> runMachine st k = runStateT (runWriterT (runM k)) st

During the course of operation we will keep a log of various state values and
internal events for debugging purposes.

> log msg = tell [msg]

Operating the `Machine`
---

The machine keeps track of the current location in the store using an `Int`.
When a new location is to be used we will simply increment this number and
return it:

> fresh :: Machine Location
> fresh = do
>     state <- get
>     let loc = mLoc state
>     put $ state { mLoc = (loc + 1) }
>     return loc

There are four basic operations on a `Machine`: inserting / looking up data in
the store / environment.

> fetch :: Location -> Machine (Maybe Value)
> fetch loc = do
>     st <- gets mStore
>     return $ IntMap.lookup loc st

> store :: Location -> Value -> Machine ()
> store loc value = do
>     state <- get
>     let sto = mStore state
>     let sto' = IntMap.insert loc value sto
>     put $ state { mStore = sto' }

> query :: Symbol -> Machine (Maybe Location)
> query sym = do
>    ev <- gets mEnv
>    return $ lookup sym ev

> bind :: Symbol -> Location -> Machine ()
> bind sym loc = do
>     state <- get
>     let ev = mEnv state
>     let ev' = (sym, loc) : ev
>     put $ state { mEnv = ev' }

Evaluating expressions
---

Evaluation is the process of reducing `Expr ()` values into `Value`s. A type
signature is worth a thousand words:

> eval :: Expr () -> Machine Value

The base case we must deal with is receiving a `Pure` value, as `Expr` is a
`Free` monad. We'll just return `VNil` for now.

> eval (Pure _) = return VNil

Top-level definitions are a little strange because they aren't strictly
"expressions" - they are mutations to the `MachineState`. When we evaluate one,
then, we will insert it into the `MachineState` and simply return `VNil`. The
state can then be reused in subsequent iterations.

> eval (Free (ADefine sym defn)) = do
>     loc <- fresh
>     store loc (VThunk defn [])
>     bind sym loc
>     log $ "defined " ++ sym
>     return VNil

Basic primitive values are handled with similar easePure:

> eval (Free (AInteger n)) = return $ VInteger n
> eval (Free (ABoolean b)) = return $ VBoolean b

Evaluating a symbol amounts to looking up the `Expr` stored at the
corresponding location and evaluating it:

> eval (Free (ASymbol s)) = do
>     loc <- query s
>     case loc of
>         Nothing -> log ("No such value: " ++ s) >> return VNil
>         Just l' -> do
>             val <- fetch l'
>             case val of
>                 Nothing -> log ("No such value: " ++ s) >> return VNil
>                 Just v' -> strict v' >>= return

A lambda abstraction amounts to updating the environment and the store
temporarily, evaluating an expression, and then reverting those changes.

Lambdas are represented by `Free (ALambda args expr)` where `args` is a list of
`Symbol`s. The goal is to create a `VClosure` with the lambda expression and an
`Environment` of all the free variables in the expression.

> eval (Free (ALambda args expr)) = do
>     vars <- variables expr
>     let vars' = vars
>     env <- forM vars' $ \var -> do
>         loc <- query var
>         case loc of
>             Nothing -> return (var, VNil)
>             Just l' -> do
>                 val <- fetch l'
>                 case val of
>                     Nothing -> return (var, VNil)
>                     Just v' -> return (var, v')
>     let lam = VClosure args expr (filter isNotNil env)
>     return lam
>         where isNotNil (_, (VNil)) = False
>               isNotNil _        = True

For now, just assume we know how to extract the variables from an `Expr`.

The language we are defining uses a *call-by-name* evaluation strategy: when
applying functions, instead of evaluating arguments and then giving them to the
function we create a thunk.

Evaluating an application, then, is the creation of a *thunk*: an unevaluated
expression along with sufficient environmental information to force evaluation
later.

> eval (Free (AApply op operands)) = do
>     fValue <- eval op >>= strict
>     ev <- gets mEnv >>= closed
>     case fValue of
>         VClosure args body env -> do
>             let env' = zip args (map (\x -> VThunk x env) operands)
>             evalWithContext (env' ++ ev) body
>         _                      ->  return fValue

Auxiliary functions
---

We have written the lion's share of the evaluation code. However, we still have
two tasks: identifying the variables in an expression, and a function `strict`
which forces thunk evaluation.

> variables :: Expr () -> Machine [Symbol]

A symbol just is a free variable:

> variables (Free (ASymbol s)) = return [s]

Function application consists of an operator and a list of arguments. For each
argument, if it is an `ASymbol`, return the underlying `Symbol`.

> variables (Free (AApply op args)) = do
>     varList <- return $ map (\(Free (ASymbol s)) -> s) $ filter isSym args
>     return varList
>         where isSym (Free (ASymbol _)) = True
>               isSym _                  = False

Lambda abstraction consists of a list of arguments and a body expression. In
this case we recurse on the body expression:

> variables (Free (ALambda args body)) = do
>     bodyVars <- variables body
>     return bodyVars

No other cases yield variables so we return an empty list.

> variables _ = return []

To force evaluation of thunks, we define a function which ensures that whatever
`Value` is put in it, the result is full evaluated (and, crucially, not a
thunk):

> strict :: Value -> Machine Value
> strict v = do
>     case v of
>         VThunk body env -> (evalWithContext env body) >>= strict
>         _               -> return v

Anytime we want to evaluate some expression with a `ClosureEnv` we must first
convert the environment to an `Environment Location`.

We achieve this by iterating over the bindings in the `ClosureEnv`, first
storing the values in the store and then returning these locations. This new
environment frame is prepended to the machine's environment, the expression is
evaluated, and then the environment is reverted to how it was before.

> evalWithContext :: ClosureEnv -> Expr () -> Machine Value
> evalWithContext env expr = do
>     state <- get
>     ev <- gets mEnv
>     newEv <- forM env $ \(var, val) -> do
>         loc <- fresh
>         store loc val
>         return (var, loc)
>     st <- gets mStore
>     put $ state { mEnv = newEv ++ ev , mStore = st}
>     ret <- eval expr
>     put $ state { mEnv = ev }
>     return ret

This function converts an `Environment Location` to a `ClosureEnv` -
effectively packaging up the lexical scope of an expression for storage in a
closure.

> closed :: Environment Location -> Machine ClosureEnv
> closed = mapM $ \(var, loc) -> do
>     val <- fetch loc
>     case val of
>         Nothing -> return (var, VNil)
>         Just v' -> return (var, v')

