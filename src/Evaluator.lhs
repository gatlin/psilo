The Virtual Machine
===

The virtual machine consists of two things:

* a statically scoped mapping from symbols to store locations
* a persistent mapping from locations to values

To that end, a Machine is a monad transformer stack with the environment
held in a Reader and the store held in a State. The execution of psilo
programs in this scheme amounts to translating Expr values to Machine
values.

The Reader monad permits function-local overwriting of the contained state
which is automatically rolled back -- precisely the behavior we want out of
our lexical environment.

The State monad, on the other hand, is persistent until the end of the
machine's execution and thus handles dynamic scope and state.

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE ExistentialQuantification #-}
>
> module Evaluator where
>
> import Control.Monad.Free
> import Prelude hiding (not,and,log,lookup)
> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Free
> import Control.Monad.Trans
> import Control.Monad.Reader
> import qualified Data.Map.Strict as Map
> import qualified Data.IntMap.Strict as IntMap
> import Data.Foldable (Foldable, fold)
> import Data.Traversable (Traversable, sequence)
> import Data.List (intersperse)
>
> import Parser
> import Syntax


The Machine
---

The machine is a monad transformer stack of a State on top of a Reader. The
State monad contains the persistent storage while the Reader monad contains the
static environment bindings.

> type Location = Int
> data Value = forall a . Show a => ClosV { symV  :: Symbol
>                                         , bodyV :: (Expr a)
>                                         , envV  :: Environment
>                                         }
>            | NumV Integer
>            | ListV [Value]
>            | NilV
> --           | BoxV { boxV :: Location } -- not ready yet
>
> deriving instance Show Value
>
> type Environment = Map.Map Symbol Int
> type Store       = IntMap.IntMap Value
>
> emptyEnv = Map.empty
> emptyStore = IntMap.empty
>
> data MStore = MStore { mStore :: Store
>                      , mLoc :: Int
>                      }
>     deriving Show
>
> data MEnv = MEnv { mEnv :: Environment }
>     deriving Show
>
> newtype Machine a = M { runM :: ReaderT MEnv (StateT MStore IO) a }
>     deriving (Monad, MonadIO, MonadState MStore, MonadReader MEnv)

Self-explanatory

> initialStore :: MStore
> initialStore = MStore { mStore = emptyStore
>                       , mLoc   = 1
>                       }

> initialEnv :: MEnv
> initialEnv = MEnv { mEnv = emptyEnv }

Run a Machine monad

> runMachine :: Machine a -> IO (a, MStore)
> runMachine k = runStateT (runReaderT (runM k) initialEnv) initialStore
>

The operation language for our machine.
---

In our view, executing this language is paramount to a transformation from
the Expr language to a machine mutation language.

We will use this language to define the translation from Expr to our
Machine.

> data OpF k
>     = Bind   Symbol Location k
>     | Lookup Symbol (Location -> k)
>     | Store  Location Value k
>     | Fetch  Location (Value -> k)
>     | Fresh  (Location -> k)
>     deriving Functor

> type Op = Free OpF

> bind :: Symbol -> Location -> Op ()
> bind s l = liftF $ Bind s l ()

> lookup :: Symbol -> Op Location
> lookup s = liftF $ Lookup s id

> store :: Location -> Value -> Op ()
> store l v = liftF $ Store l v ()

> fetch :: Location -> Op Value
> fetch l = liftF $ Fetch l id

> fresh :: Op Location
> fresh = liftF $ Fresh id

> lookupVar :: Symbol -> MEnv -> Location
> lookupVar sym (MEnv env) = env Map.! sym

> bindVar :: Symbol -> Location -> MEnv -> MEnv
> bindVar sym loc (MEnv env) = MEnv $ Map.insert sym loc env

Build a Machine computation out of Ops

> runOp :: Op a -> Machine a
> runOp (Pure v) = return v

Bind a location to a symbol

> runOp (Free (Bind sym loc next)) = do
>     env <- ask
>     local (bindVar sym loc) $ runOp next

Lookup a location for the given symbol

> runOp (Free (Lookup sym next)) = do
>     loc <- asks (lookupVar sym)
>     runOp $ next loc

Store a value in a given location

> runOp (Free (Store loc val next)) = do
>     state  <- get
>     sto    <- return $ mStore state
>     sto'   <- return $ IntMap.insert loc val sto
>     put $ state { mStore = sto' }
>     runOp next

Fetch a value from a given location

> runOp (Free (Fetch loc next)) = do
>     state  <- get
>     sto    <- return $ mStore state
>     val    <- return $ sto IntMap.! loc
>     runOp $ next val

Increment the current location and return the most recent unused one

> runOp (Free (Fresh next)) = do
>     state <- get
>     loc   <- return $ mLoc state
>     put $ state { mLoc = (loc + 1) }
>     runOp $ next loc

> opTest :: Machine ()
> opTest = runOp $ do
>     loc1 <- fresh
>     bind "huh" loc1
>     store loc1 $ NumV 5
>     (NumV val) <- lookup "huh" >>= fetch
>     bind "huh" 0
>     store 0 $ NumV (val * 2)
>     return ()

Interpretation
---

Now that we have a static environment and a dynamic store,
it is time to actually interpret programs.

We do so by translating monadic Expr values into monadic Machine values.
During this translation, our monad transformer stack allows us to manage and
orchestrate the necessary side effects.

> interpret :: Show a => Expr a -> Machine Value
> interpret (Pure v) = return NilV

> interpret (Free (AInteger n)) = return $ NumV n
> interpret (Free (ASymbol  s)) = do
>     val <- runOp $ lookup s >>= fetch
>     return val

> interpret (Free (AList xs)) = do
>     vals <- forM xs $ \x -> do
>         x' <- return x
>         let v = interpret x'
>         v
>     return $ ListV vals

> interpret (Free (AAdd args)) = do
>     (ListV args')    <- interpret args
>     (NumV l) <- return $ args' !! 0
>     (NumV r) <- return $ args' !! 1
>     return $ NumV $ l + r

> interpret (Free (AMult args)) = do
>     (ListV args')    <- interpret args
>     (NumV l) <- return $ args' !! 0
>     (NumV r) <- return $ args' !! 1
>     return $ NumV $ l * r

### A note on lambdas

The below code for lambdas, while technically correct, has a huge problem: it
copies its *entire* environment. A much smarter trick would be to only copy
that which is actually used.

Also, function application is currently very stupid. It is intended that
functions will take one argument, a list containing the actual values to be
processed. At the moment this is not honored, but the process is simple:

1. If the operand list is sufficiently long, zip it with the list of symbols in
the function.

2. Create an `Environment` out of this zipped list.

3. Form a union between this new environment and the current, favoring the new
one.

`ClosV` and `ALambda` will need to change to accept a list of arguments, but
this is trivial.

> interpret (Free (ALambda arg body)) = do
>     (MEnv currentEnv) <- ask
>     return $ ClosV arg body currentEnv

> interpret (Free (AApply fun arg)) = do
>     (ClosV sym body env) <- interpret fun
>     (MEnv currentEnv)    <- ask
>     newLoc  <- runOp $ fresh
>     (ListV argVal)  <- interpret arg
>     runOp $ store newLoc (argVal !! 0)
>     env'    <- return $ Map.insert sym newLoc env
>     local (\(MEnv e) -> MEnv (Map.union env' e)) $ do
>         interpret body
