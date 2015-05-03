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
our environment.

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
> {-# LANGUAGE FlexibleContexts #-}
>
> module Evaluator
>
> ( Machine(..)
> , MachineState(..)
> , Value(..)
> , runMachine
> , newMachineState
> , fetch
> , query
> , update
> , pushFrame
> , popFrame
> , fresh
> , load
> , store
> , eval
> , variables
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
> import Data.List (intersperse, nub, (\\), concat)
> import Data.Monoid
> import Control.Applicative
>
> import Parser
> import Syntax
> import Typechecker

Values
---

Borrowing (stealing?) from Krishnamurthi's inimitable [Programming Languages:
Application and Interpretation][plai] the environment does not map symbols to
values but to *locations* in the store. The store, then, maps location to
values.

[plai]: http://cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/

What is the result of evaluating an `Expr` with a `Machine`? There should be
some ultimate result type onto which we can map our `Expr`s.

> data Value
>     = VNil
>     | VInteger Integer
>     | VBoolean Bool
>     | VClosure { clArgs :: [Symbol]
>                , clBody :: (Expr ())
>                , clEnv  :: Closed }
>     | VThunk (Expr ()) Closed -- ^ Suspended computation
>     deriving (Eq)

> instance Show Value where
>     show VNil = "()"
>     show (VBoolean b) = show b
>     show (VInteger n) = show n
>     show (VThunk   e c) = "<thunk> { " ++ (show e) ++ " , " ++
>                                           (show c) ++ " } "
>     show (VClosure a b e) = "<closure> { args = " ++ (show a) ++
>                             ", body = " ++ (show b) ++
>                             ", env  = " ++ (show e) ++
>                             " } "

The Machine
---

A machine is an environment and a store at minimum. For convenience, our
machine will also maintain a monotonically increasing integer value to be used
for new symbols.

For simplicity, an environment is a list of tuples and a store is an `IntMap`:

> type Environment a = [(Symbol, a)]
> type Store = IntMap.IntMap Value

A location in the store is just an `Int` but for readability I'll define a type
alias:

> type Location = Int

Also to make the code more readable, an environment mapping `Symbol`s to `Expr ()`
values is called a `Closed` environment:

> type Closed = Environment Value

Since we will often want to push and pop several entries at a time, the machine
will keep a list of `Environment`s. Thus our machine's state:

> data MachineState = MachineState
>     { mEnv    :: [Environment Location]
>     , mSto    :: Store
>     , mLoc    :: Location
>     } deriving (Show)

> newMachineState = MachineState [] IntMap.empty 0

With our state defined clearly we may define our `Machine` type, a composition
of `Reader`, `Writer`, and `State` monad transformers:

> newtype Machine a = M {
>     runM :: WriterT [String] (StateT MachineState IO) a
> } deriving (Monad, MonadIO, MonadState MachineState
>            , MonadWriter [String], Functor, Applicative)

> runMachine :: MachineState -> Machine a -> IO ((a, [String]), MachineState)
> runMachine st k = runStateT (runWriterT (runM k)) st

For logging convenience, I'll write a simple logging command:

> log msg = tell [msg] -- provided by @WriterT@

There are four fundamental operations we must define to use our machine:
querying the environment, fetching from the store, binding symbols in the
environment, and updating the store.

Fetching from the store is straightforward thanks to the good folks who wrote
`IntMap`:

> fetch :: Location -> Machine (Maybe Value)
> fetch loc = do
>     st <- gets mSto
>     return $ IntMap.lookup loc st

Querying the environment is a little more involved. If a symbol has no binding
in the top frame, the remainder of the frame list is searched.

> query :: Symbol -> Machine (Maybe Location)
> query sym = gets mEnv >>= return . go
>    where go []      = Nothing
>          go (e:es)  = case lookup sym e of
>                           (Just v) -> Just v
>                           Nothing  -> go es

Once again, updating the store is a simple affair:

> update :: Location -> Value -> Machine ()
> update loc value = do
>     sto <- gets mSto
>     let sto' = IntMap.insert loc value sto
>     modify $ \st -> st { mSto = sto' }

And completing the cycle, binding values warrants some consideration. To bind a
symbol to a location is tantamantout pushing a new environment frame onto the
frame stack.

> pushFrame :: Environment Location -> Machine ()
> pushFrame frame = do
>     ev <- gets mEnv
>     let ev' =  frame : ev
>     modify $ \st -> st { mEnv = ev' }

If we unbind symbols and locations we should make sure to remove the
corresponding values in the store:

> popFrame :: Machine ()
> popFrame = do
>     (popped:frames) <- gets mEnv
>     forM_ popped $ \(sym, loc) -> do
>         sto <- gets mSto
>         let sto' = IntMap.delete loc sto
>         modify $ \st -> st { mSto = sto' }
>     modify $ \st -> st { mEnv = frames }

Because very often we will simply want to put a value in memory and refer to it
later, the following two commands wrap up the process of loading and storing
values:

> load :: Symbol -> Machine (Maybe Value)
> load sym = do
>     maybeLoc <- query sym
>     case maybeLoc of
>         Just loc -> fetch loc >>= return
>         Nothing  -> return Nothing
>
> store :: Symbol -> Value -> Machine ()
> store sym val = do
>     loc <- fresh
>     update loc val
>     pushFrame [(sym, loc)]

As a convenience, it would also be nice to obtain fresh store locations on
demand. Voici:

> fresh :: Machine Location
> fresh = do
>     loc <- gets mLoc
>     modify $ \st -> st { mLoc = (loc + 1) }
>     return loc

Evaluating expressions to values
---

The process of converting a set of psilo expressions into a final result value
is *evaluation*. This evaluator implements *call-by-name* semantics: when a
function is applied to its arguments, the expression is only evaluated to the
outermost closure and stored as a *thunk*.

This is in contrast to *call-by-value* semantics: when a function is applied to
its arguments, the arguments are fully evaluated and then the function is fully
evaluated.

Both have their advantages and disadvantages. `Expr` is a free monad, meaning
that you get to determine the strategy for tearing it down and converting it to
something else. The upshot is, I could easily write a call-by-value evaluator
for the same program. In fact I might!

To begin, let's handle the type signature and the trivial case:

> eval :: Expr () -> Machine Value
> eval (Pure _) = return VNil

Top-level definitions are a little strange because they aren't expressions -
they are mutations to the machine's state (or "statements").

Our evaluation strategy will be to store them as thunks bound to the name they
were given.

> eval (Free (ADefine sym defn)) = do
>     store sym (VThunk defn [])
>     log $ "Defined " ++ sym
>     return VNil

Basic primitive values are handled with similar ease:

> eval (Free (AInteger n)) = return $ VInteger n
> eval (Free (ABoolean b)) = return $ VBoolean b

Evaluating a symbol amounts to looking up the value at the corresponding
location and then evaluating it:

> eval (Free (ASymbol s)) = do
>     maybeValue <- load s
>     case maybeValue of
>         Nothing -> log ("No such value: " ++ s) >> return VNil
>         Just val -> strict val >>= return

What is `strict`? Strict takes any `Value` and, if that value is a thunk,
forces its evaluation. It is defined in the following section.

Lambdas are lists of symbols (the arguments), an expression into which the
argument values will be substituted (the body), and the environment surrounding
them at the time they are created (the closed environment).

We will convert lambdas into `VClosure` values. The surrounding environment
will be packaged up naively for now; more intelligent selection of values will
be implemented in due time.

> eval (Free (ALambda args body)) = do
>     log "Evaluating a closure ..."
>     env <- gets mEnv
>     vars <- variables body
>     env' <- forM (concat env) $ \(sym, loc) -> do -- create a `Closed`
>         Just val <- load sym
>         return (sym, val)
>     return $ VClosure args body env'

Application is the real tricky part of the evaluator. We will evaluate the
expression but not its arguments, and instead package them into thunks.

> eval (Free (AApply op operands)) = do
>     log $ "Applying " ++ (show op) ++ " to " ++ (show operands)
>     case op of
>         (Free (ASymbol "+")) -> builtinAdd operands
>         (Free (ASymbol "*")) -> builtinMul operands
>         (Free (ASymbol "-")) -> builtinSubtract (operands !! 0)
>                                                 (operands !! 1)
>         (Free (ASymbol "/")) -> builtinDiv (operands !! 0)
>                                            (operands !! 1)
>         _ -> do
>             fValue <- eval op >>= strict
>             case fValue of
>                 (VClosure args body clEnv) -> do
>                     log $ "Evaluating a closure ..."
>                     ev <- gets mEnv >>= close . concat
>                     operands' <- forM operands $ \o -> return $ VThunk o ev
>                     evalWithContext (nub ((zip args operands') ++ clEnv)) body
>                 _ -> do
>                     log $ "Applying " ++ (show fValue)
>                     return fValue

Builtins
---

There are a few builtin operators and functions defined for convenience:

> builtinAdd :: [Expr ()] -> Machine Value
> builtinAdd operands = do
>     operands' <- forM operands $ \(Free operand) -> do
>         case operand of
>             AInteger x -> return x
>             s          -> eval (Free s) >>= strict >>= go
>     return $ VInteger $ sum operands'
>     where go (VInteger n) = return n
>           go huh          = do
>               log $ "[+] wtf : " ++ (show huh)
>               return 0

> builtinMul :: [Expr ()] -> Machine Value
> builtinMul operands = do
>     operands' <- forM operands $ \(Free operand) -> do
>         case operand of
>             AInteger x -> return x
>             s          -> eval (Free s) >>= strict >>= \(VInteger x) -> return x
>     return $ VInteger $ product operands'

> builtinSubtract :: Expr () -> Expr () -> Machine Value
> builtinSubtract a b = do
>     (a':b':_) <- forM [a, b] $ \(Free operand) -> do
>         case operand of
>             AInteger x -> return x
>             s          -> eval (Free s) >>= strict >>= \(VInteger x) -> return x
>     return $ VInteger $ a' - b'

> builtinDiv :: Expr () -> Expr () -> Machine Value
> builtinDiv a b = do
>     (a':b':_) <- forM [a, b] $ \(Free operand) -> do
>         case operand of
>             AInteger x -> return x
>             s          -> eval (Free s) >>= strict >>= \(VInteger x) -> return x
>     return $ VInteger $ a' `div` b'

Auxiliary functions
---

> strict :: Value -> Machine Value
> strict (VThunk body env) = (evalWithContext env body) >>= strict
> strict v                 = return v

> evalWithContext :: Closed -> Expr () -> Machine Value
> evalWithContext env expr = do
>     newEv <- forM env $ \(var, val) -> do
>         loc <- fresh
>         update loc val
>         return (var, loc)
>     pushFrame newEv
>     ret <- eval expr
>     popFrame
>     return ret

> close :: Environment Location -> Machine Closed
> close ev = forM ev $ \ (var, loc) -> do
>     Just val <- load var
>     return (var, val)

> variables :: Expr a -> Machine [Symbol]
> variables (Free (ASymbol s)) = return [s]
> variables (Free (ALambda args body)) = do
>     bodyVars <- variables body
>     return bodyVars
> variables (Free (AApply op args)) = do
>     varList <- mapM variables args
>     return $ concat varList
> variables _ = return []
