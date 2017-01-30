{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
This is heavily inspired by

    https://github.com/taiki45/hs-vm/

Some terminology has changed, all values are unsigned 16 bit words,
the ISA has been expanded, and ultimately the machine will have some
rudimentary FFI / IO capabilities.
-}

module Lib.Machine where

import Control.Applicative
import Data.Functor.Identity
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromJust)
import Data.Word

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State

import Lib.Syntax

type Location = Int
type Value = Word16

-- | Mapping from symbols to locations. Serves double duty in the VM as both the
-- type of closure environments (hence its name) and the mapping of labels to
-- addresses.
newtype Env = Env {
    unEnv :: Map Symbol Location
    } deriving (Show)

instance Monoid Env where
    mempty = Env M.empty
    (Env e1) `mappend` (Env e2) = Env $ M.union e1 e2

envSet :: Symbol -> Location -> Env -> Env
envSet sym val (Env env) = Env $ M.insert sym val env

envGet :: Symbol -> Env -> Location
envGet sym (Env env) = fromJust $ M.lookup sym env

-- * The Machine

-- | A simple list-based stack implementation for the machine
newtype Stack = Stack {
    unStack :: [Value]
    } deriving (Show)

stackPush :: Value -> Stack -> Stack
stackPush val (Stack mem) = Stack $ val:mem

stackPop :: Stack -> Stack
stackPop (Stack (x:xs)) = Stack xs

stackPeek :: Stack -> Value
stackPeek (Stack (x:_)) = x

-- | Apply the operator to the first two stack values as operands
stackBinOp :: (Value -> Value -> Value) -> Stack -> Stack
stackBinOp op (Stack (x:y:rest)) = Stack $ op x y : rest

stackSwap :: Stack -> Stack
stackSwap (Stack (x:y:rest)) = Stack $ y : (x : rest)

stackApp :: (Value -> Value) -> Stack -> Stack
stackApp f (Stack (x:xs)) = Stack $ f x : xs

newStack :: Stack
newStack = Stack []

instance Monoid Stack where
    mempty = newStack
    (Stack s1) `mappend` (Stack s2) = Stack $ s1 `mappend` s2

-- | Local Stack
newtype LocalStack = LocalStack {
    unLocalStack :: [Value]
    } deriving (Show)

localPush :: Value -> LocalStack -> LocalStack
localPush v (LocalStack vs) = LocalStack $ v : vs

localPop :: LocalStack -> (Value, LocalStack)
localPop (LocalStack (v:vs)) = (v, LocalStack vs)

localPeek :: LocalStack -> Value
localPeek (LocalStack (x:_)) = x

newLocalStack :: LocalStack
newLocalStack = LocalStack []

instance Monoid LocalStack where
    mempty = newLocalStack
    (LocalStack l1) `mappend` (LocalStack l2) = LocalStack $
        l1 `mappend` l2

-- | A vector-based heap implementation
newtype Heap = Heap {
    unHeap :: Vector Value
    }

instance Show Heap where
    show (Heap mem) = "Heap<" ++ (show $ V.length mem) ++ ">"

heapSet :: Location -> Value -> Heap -> Heap
heapSet loc val (Heap mem) = Heap $ mem V.// [(loc, val)]

heapGet :: Location -> Heap -> Value
heapGet loc (Heap mem) = mem ! loc

newHeap :: Int {- ^ Size -} -> Heap
newHeap sz = Heap $ V.replicate sz 0

-- | Call stack. Not a newtype because ultimately the machine is the wrapper
-- around this structure.
type CallStack = [(Location, LocalStack)]

newCallStack :: CallStack
newCallStack = []

-- | The actual machine definition
data MachineState = M
    { machinePC :: Location
    , machineStack :: Stack
    , machineHeap :: Heap
    , machineCallStack :: CallStack
    , machineLocalStack :: LocalStack
    , machineLabels :: Env
    } deriving (Show)

newtype MachineT m a = Machine {
    unMachineT :: StateT MachineState m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState MachineState
               , MonadTrans )

type Machine = MachineT IO

runMachine :: Monad m => MachineState -> MachineT m a -> m (a, MachineState)
runMachine ms m = runStateT (unMachineT m) ms

newMachineState :: Int {- ^ Heap size -} -> MachineState
newMachineState heapSz = M
    0
    newStack
    (newHeap heapSz)
    newCallStack
    newLocalStack
    mempty

setCounter :: Int -> MachineState -> MachineState
setCounter c (M _ st hp cs ls env) = M c st hp cs ls env

callStackPush :: Int -> MachineState -> MachineState
callStackPush c (M pc st hp cs ls env) =
    M pc st hp ((c, ls):cs) newLocalStack env

callStackPop :: MachineState -> (Int, MachineState)
callStackPop (M pc st hp ((c,ls):cs) _ env) =
    (c,M pc st hp cs ls env)

incrPC :: MachineState -> MachineState
incrPC (M pc st hp cs ls env) =
    M (pc + 1) st hp cs ls env

mapStack :: (Stack -> Stack) -> MachineState -> MachineState
f `mapStack` (M pc st hp cs ls env) =
    M pc (f st) hp cs ls env

mapHeap :: (Heap -> Heap) -> MachineState -> MachineState
f `mapHeap` (M pc st hp cs ls env) =
    M pc st (f hp) cs ls env

mapLocal :: (LocalStack -> LocalStack) -> MachineState -> MachineState
f `mapLocal` (M pc st hp cs ls env) =
    M pc st hp cs (f ls) env

mapLabels :: (Env -> Env) -> MachineState -> MachineState
f `mapLabels` (M pc st hp cs ls env) =
    M pc st hp cs ls (f env)

-- * Low level instructions

-- | The assembly instructions for the stack machine
data Asm
    = Add             -- ^ Add stack values
    | Sub             -- ^ Subtract stack values
    | Mul             -- ^ Multiply stack values
    | Div             -- ^ Integer division of stack values
    | Lt              -- ^ first stack value < second stack value
    | Le              -- ^ first stack value <= second stack value
    | Gt              -- ^ first stack value > second stack value
    | Ge              -- ^ first stack value >= second stack value
    | Eq              -- ^ first stack value == second stack value
    | Not             -- ^ Turnover bool: non-zero => 0, 0 => 1
    | StoreI Location -- ^ Store top of stack to immediate location
    | LoadI Location  -- ^ Push value at immediate location onto stack
    | Store           -- ^ See [1] below this definition
    | Load            -- ^ See [1] below this definition
    | Push Value      -- ^ Push constant value to stack
    | Pop             -- ^ Pop and discard value from stack
    | Dup             -- ^ Duplicate top value on stack
    | Swap            -- ^ Swap the top two values on the stack
    | PushLocal       -- ^ Push value to local stack
    | PopLocal        -- ^ Pop value from local stack
    | Label Symbol    -- ^ Set label and save current position
    | Jump Symbol     -- ^ Unconditional jump
    | JumpIf Symbol   -- ^ Jump if first stack value is non-zero. Then discard.
    | Call Symbol     -- ^ Store current PC and jump to function.
    | Ret             -- ^ Return from calling point
    deriving (Show)

{-
[1]: Like 'StoreI' and 'LoadI' but the locations are not hardcoded, but instead
the top stack value is the storage location, and the second is the actual value.
The location is *not* discarded.
-}

-- | Top level instruction execution.
execute :: Monad m => Asm -> MachineT m ()
execute i@(Jump _)= execute' i
execute i@(JumpIf _) = execute' i
execute i@(Call _) = execute' i
execute (Ret) = execute' Ret
execute i = execute' i >> modify incrPC

setLabel :: Asm -> MachineState -> MachineState
setLabel (Label l) m = incrPC $ const (envSet l c ls) `mapLabels` m
    where c = machinePC m
          ls = machineLabels m
setLabel _ m = incrPC m

execute' :: Monad m => Asm -> MachineT m ()
execute' (Add) = modify (mapStack (stackBinOp (+))) >> return ()
execute' (Sub) = modify (mapStack (stackBinOp (-))) >> return ()
execute' (Mul) = modify (mapStack (stackBinOp (*))) >> return ()

execute' (Lt) = modify (mapStack (stackBinOp (boolToValue <.> (<))))

execute' (Le) = modify (mapStack (stackBinOp (boolToValue <.> (<=))))

execute' (Gt) = modify (mapStack (stackBinOp (boolToValue <.> (>))))

execute' (Ge) = modify (mapStack (stackBinOp (boolToValue <.> (>=))))

execute' (Eq) = modify (mapStack (stackBinOp (boolToValue <.> (==))))

execute' (Not) = do
    let invNum n = case n of
            0 -> 1
            _ -> 0
    modify $ mapStack (stackApp invNum)
    return ()

execute' (StoreI loc) = do
    stack <- gets machineStack
    modify $ mapHeap (heapSet loc (stackPeek stack))

execute' (LoadI loc) = do
    heap <- gets machineHeap
    modify $ mapStack (stackPush (heapGet loc heap))

execute' (Store) = do
    m <- get
    let (loc, val) = (\(Stack (x:y:_)) -> (x, y)) $ machineStack m
    put $ heapSet (fromIntegral loc) val `mapHeap` m

execute' (Load) = do
    stack <- gets machineStack
    heap <- gets machineHeap
    let loc = stackPeek stack
    let loc' = fromIntegral loc
    modify $ mapStack (stackPush (heapGet loc' heap))
    return ()

execute' (Push v) = modify $ mapStack (stackPush v)
execute' (Pop) = modify $ mapStack stackPop
execute' (Dup) = do
    stack <- gets machineStack
    modify $ mapStack (stackPush (stackPeek stack))

execute' (Swap) = modify $ mapStack stackSwap

execute' (PushLocal) = do
    stack <- gets machineStack
    modify $ mapStack stackPop .
        mapLocal (localPush (stackPeek stack))

execute' (PopLocal) = do
    m <- get
    let stack = machineStack m
    let local = machineLocalStack m
    let (v, lds) = localPop local
    modify $ mapLocal (const lds) . mapStack (stackPush v)

execute' (Label _) = return ()

execute' (Jump n) = do
    labels <- gets machineLabels
    modify $ setCounter (envGet n labels)

execute' (JumpIf n) = do
    m <- get
    let stack = machineStack m
        labels = machineLabels m
        popped = stackPop `mapStack` m
    if stackPeek stack == 0
        then put $ incrPC popped
        else put $ setCounter (envGet n labels) popped

execute' (Call n) = do
    m <- get
    let current = machinePC m
        labels = machineLabels m
        target = envGet n labels
    modify $ setCounter target . callStackPush current

execute' (Ret) = do
    m <- get
    let (callPoint, m') = callStackPop m
    put $ setCounter (callPoint + 1) m'

boolToValue :: Bool -> Value
boolToValue False = 0
boolToValue True = 1

(<.>) :: (a -> Value) -> (Value -> Value -> a) -> Value -> Value -> Value
(<.>) f op v w = f $ op v w

-- | Run a program on a brand new machine

prepare :: [Asm] -> MachineState -> MachineState
prepare is = appEndo . getDual $ foldMap (Dual . Endo . setLabel) is

setMain :: MachineState -> MachineState
setMain m = setCounter _main m
    where _main = envGet "main" $ machineLabels m

run :: [Asm] -> IO MachineState
run is = run' (V.fromList $ execute <$> is)
    (setMain . setCounter 0 . prepare is $ newMachineState 65536)

run'
    :: Vector (Machine ())
    -> MachineState
    -> IO MachineState
run' is ms
    | end pc = return ms
    | otherwise = do
          (_, ms') <- runMachine ms (is ! pc)
          run' is ms'
    where pc = machinePC ms
          end = ((V.length is) ==)

{-
run'
    :: Vector (MachineState -> Identity MachineState)
    -> MachineState
    -> IO MachineState
run' is m
    | end pc = return m
    | otherwise = (is ! pc $ m) >>= \m' -> run' is m'
    where pc = machinePC $ m
          end = ((V.length is) ==)
-}

-- * Some tests

test1 :: [Asm]
test1 = [ Label "sub5"
        , PushLocal
        , Push 5
        , PopLocal
        , Sub
        , Ret
        , Label "main"
        , Push 11
        , Call "sub5"
        ]

test2 :: [Asm]
test2 = [ Label "sub5"
        , PushLocal
        , Push 5
        , PopLocal
        , Sub
        , Ret
        , Label "store_value"
        -- assumption: stack is address and then value
        , Store
        , Pop
        , Pop
        , Ret
        , Label "main"
        , Push 6
        , Call "sub5"
        , Push 17
        , Call "store_value"
        ]

-- non-tail-recursive fibonacci
test3 :: [Asm]
test3 = [ Label "fib"           -- "n" is on the stack
        , Dup                   -- duplicate it
        , Push 0              -- push a 0
        , Eq                    -- replace with 1 if n=0, else 0
        , JumpIf "fib_finished" -- if 1, n=0 and we are done
        , Dup                   -- duplicate "n" again
        , Push 1                -- push a 1
        , Eq                    -- compare again
        , JumpIf "fib_finished" -- if 1, n=1 and we are finished
        , Jump "fib_continue"   -- continue
        , Label "fib_finished"  -- JumpIf pops the stack, n is already at top
        , Ret                   -- ... so return it
        , Label "fib_continue"
        , Dup                   -- duplicate "n"
        , PushLocal             -- \
        , Push 1                --  +- put a 1 on the stack between the "n"s
        , PopLocal              -- /
        , Sub                   -- subtract, putting "n-1" on the stack
        , PushLocal             -- set this aside
        , PushLocal             -- \
        , Push 2                --  +- put a 2 on the stack before the "n"
        , PopLocal              -- /
        , Sub                   -- subtract, putting "n-2" on the stack
        , PopLocal              -- put the "n-1" back on the stack
        , Call "fib"            -- call fib(n-1)
        , PushLocal             -- set the result aside
        , Call "fib"            -- call fib(n-2), since "n-2" is on the stack
        , PopLocal              -- push fib(n-1) back on the stack
        , Add                   -- add these two together
        , Ret                   -- return
        , Label "main"
        , Push 10
        , Call "fib"
        ]

-- tail-recursive fibonacci
test4 :: [Asm]               -- top of stack <-----> bottom of stack
test4 = [ Label "fib"        -- n
        , PushLocal          -- _
        , Push 0             -- 0
        , Push 1             -- 1 -> 0
        , PopLocal           -- n -> 1 -> 0
        , Call "fib_rec"     -- n -> A -> B
        , Pop                -- A -> B
        , Pop                -- B
        , Ret
        , Label "fib_rec"    -- n -> A -> B
        , Dup                -- n -> n -> A -> B
        , Push 0             -- 0 -> n -> n -> A -> B
        , Eq                 -- t/f -> n -> A -> B
        , JumpIf "fib_done"  -- n -> A -> B
        , PushLocal          -- A -> B
        , Dup                -- A -> A -> B
        , PushLocal          -- A -> B
        , Add                -- (A+B)
        , PopLocal           -- A -> (A+B)
        , Swap               -- (A+B) -> A
        , Push 1             -- 1 -> (A+B) -> A
        , PopLocal           -- n -> 1 -> (A+B) -> A
        , Sub                -- (n-1) -> (A+B) -> A
        , Jump "fib_rec"
        , Label "fib_done"   -- n -> A -> B
        , Ret
        , Label "main"
        , Push 8
        , Call "fib"
        ]

-- non-tail-recursive factorial
test5 :: [Asm]
test5 = [ Label "fact"
        , Dup
        , Push 1
        , Eq
        , JumpIf "fact_ret"
        , Jump "fact_continue"
        , Label "fact_ret"
        , Ret
        , Label "fact_continue"
        , Dup
        , Push 1
        , Swap
        , Sub
        , Call "fact"
        , Mul
        , Ret
        , Label "main"
        , Push 5
        , Call "fact"
        ]

-- tail-recursive factorial
test6 :: [Asm]
test6 = [ Label "fact"
        , PushLocal
        , Push 1
        , PopLocal
        , Call "fact_helper" -- (fact-helper 1 n)
        , Ret
        , Label "fact_helper"
        , Dup  -- stack contains "product" and "n" and "n"
        , PushLocal
        , Push 2
        , PopLocal
        , Lt
        , JumpIf "fact_helper_ret"
        , Dup
        , Push 1
        , Swap
        , Sub
        , PushLocal
        , Mul
        , PopLocal
        , Jump "fact_helper"
        , Label "fact_helper_ret"
        , Pop
        , Ret
        , Label "main"
        , Push 5
        , Call "fact"
        ]

test7 :: [Asm]
test7 = [ Label "main"
        , Push 10
        , Push 17
        , Store
        , Load
        ]
