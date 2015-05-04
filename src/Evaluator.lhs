---
title: "The psilo Virtual Machine"
...

The virtual machine consists of two things:

* a statically scoped mapping from symbols to store locations (the "environment"); and
* a persistent mapping from locations to values (the "store").

The State monad is used to keep track of changes to the environment and the
store. It is persistent through the life of the computation.

The Writer monad is used for logging purposes.

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
> , Location
> , runMachine
> , newMachineState
> , fetch
> , query
> , update
> , bind
> , fresh
> , load
> , store
> , eval
> , strict
> , freeVariables
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
> import Data.List (intersperse, nub, (\\), concat, intersect)
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
>                , clEnv  :: Environment Location}
>     | VThunk (Expr ()) (Environment Location) -- ^ Suspended computation
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
>     { mEnv    :: Environment Location
>     , mSto    :: Store
>     , mLoc    :: Location
>     } deriving (Show)

> newMachineState = MachineState [] IntMap.empty 0

With our state defined clearly we may define our `Machine` type, a composition
of `Writer`, and `State` monad transformers:

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
> query sym = gets mEnv >>= \env -> return $ lookup sym env

Once again, updating the store is a simple affair:

> update :: Location -> Value -> Machine ()
> update loc value = do
>     sto <- gets mSto
>     let sto' = IntMap.insert loc value sto
>     modify $ \st -> st { mSto = sto' }

And completing the cycle, binding values warrants some consideration. To bind a
symbol to a location is tantamantout pushing a new environment frame onto the
frame stack.

> bind :: Symbol -> Location -> Machine ()
> bind sym loc = do
>     ev <- gets mEnv
>     let ev' =  (sym, loc) : ev
>     modify $ \st -> st { mEnv = ev' }

Because very often we will simply want to put a value in memory and refer to it
later, the following two commands wrap up the process of loading and storing
values:

> load :: Symbol -> Machine (Maybe Value)
> load sym = do
>     maybeLoc <- query sym
>     case maybeLoc of
>         Just loc -> fetch loc >>= return
>         Nothing  -> do
>             sto <- gets mSto
>             log $ "Can't find " ++ (show sym)
>             log $ "Current store: " ++ (show sto)
>             return Nothing
>
> store :: Symbol -> Value -> Machine ()
> store sym val = do
>     loc <- fresh
>     update loc val
>     bind sym loc

As a convenience, it would also be nice to obtain fresh store locations on
demand. Voici:

> fresh :: Machine Location
> fresh = do
>     loc <- gets mLoc
>     modify $ \st -> st { mLoc = (loc + 1) }
>     return loc

Evaluating expressions to values
---

> eval :: Expr () -> Machine Value
> eval (Pure _) = return VNil

> eval (Free (ADefine sym defn)) = do
>     v <- eval defn
>     store sym v
>     return VNil

> eval (Free (AInteger n)) = return $ VInteger n
> eval (Free (ABoolean b)) = return $ VBoolean b

> eval (Free (ASymbol s)) = do
>     log $ "Looking up symbol " ++ (show s)
>     maybeVal <- load s
>     case maybeVal of
>         Just v -> strict v
>         Nothing -> do
>             log "value not found"
>             return $ VInteger 0

> eval (Free (ALambda args body)) = do
>     ev <- gets mEnv
>     let vars = freeVariables body
>     let ev' = filter (\(sym, val) -> elem sym vars) ev
>     return $ VClosure args body ev'

> eval (Free (AApply op erands)) = case builtin op of
>     Just op' -> op' erands
>     Nothing -> do
>         fValue <- eval op >>= strict
>         log $ "fValue = " ++ (show fValue)
>         case fValue of
>             (VClosure args body cl) -> do
>                 let diff = (length args) - (length erands)
>                 ev <- gets mEnv
>                 erands' <- forM erands $ \o -> do
>                     loc <- fresh
>                     update loc $ VThunk o ev
>                     return loc
>                 if (diff > 0)
>                     then return $
>                            VClosure (drop diff args)
>                            body $
>                            (zip (take diff args) erands') ++ cl
>                 else do
>                     let ev' = (zip args erands') ++ cl
>                     log $ "new environment: " ++ (show ev')
>                     modify $ \st -> st { mEnv = ev' }
>                     result <- eval body
>                     modify $ \st -> st { mEnv = ev }
>                     return result
>             _ -> return VNil

> strict :: Value -> Machine Value
> strict (VThunk body cl) = do
>     oldEnv <- gets mEnv
>     modify $ \st -> st { mEnv = cl }
>     result <- eval body
>     modify $ \st -> st { mEnv = oldEnv }
>     return result
> strict v = return v

> builtin (Free (ASymbol sym)) = case sym of
>     "+" -> Just $ \operands -> do
>         operands' <- cleanse operands
>         return $ VInteger $ sum operands'
>     "*" -> Just $ \operands -> do
>         operands' <- cleanse operands
>         return $ VInteger $ product operands'
>     "-" -> Just $ \operands -> do
>         operands' <- cleanse operands
>         return $ VInteger $ (operands' !! 0) - (operands' !! 1)
>     "/" -> Just $ \operands -> do
>         operands' <- cleanse operands
>         return $ VInteger $ (operands' !! 0) `div` (operands' !! 1)
>     "=?" -> Just $ \operands -> do
>         operands' <- cleanse operands
>         return $ VBoolean $ (operands' !! 0) == (operands' !! 1)
>     "if" -> Just $ \operands -> do
>         VBoolean c <- eval (operands !! 0) >>= strict
>         if c then eval (operands !! 1) >>= strict
>              else eval (operands !! 2) >>= strict
>     _ -> Nothing
>     where cleanse xs = forM xs $ \o -> do
>               s <- eval o >>= strict
>               case s of
>                   VInteger n -> return n
>                   other -> do
>                       log $ "wtf, got " ++ (show other)
>                       return 0
> builtin _ = Nothing

> freeVariables :: Expr () -> [Symbol]
> freeVariables (Free (ALambda args body)) = (freeVariables body) \\ args
> freeVariables (Free (AApply op erands)) =
>     (freeVariables op) ++ (concatMap freeVariables erands)
> freeVariables (Free (ASymbol s)) = [s]
> freeVariables _ = []
