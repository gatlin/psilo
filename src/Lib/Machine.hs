{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

{- |
This is heavily inspired by

    https://github.com/taiki45/hs-vm/

Some terminology has changed, all values are unsigned 16 bit words,
the ISA has been expanded, and ultimately the machine will have some
rudimentary FFI / IO capabilities.
-}

module Lib.Machine where

import Control.Comonad
import Control.Applicative
import Data.Functor.Identity
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromJust)
import Data.Word

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
data Machine = Machine
    { machinePC :: Location
    , machineStack :: Stack
    , machineHeap :: Heap
    , machineCallStack :: CallStack
    , machineLocalStack :: LocalStack
    , machineLabels :: Env
    } deriving (Show)

newMachine :: Int {- ^ Heap size -} -> Machine
newMachine heapSz = Machine
    0
    newStack
    (newHeap heapSz)
    newCallStack
    newLocalStack
    mempty

setCounter :: Int -> Machine -> Machine
setCounter c (Machine _ st hp cs ls env) = Machine c st hp cs ls env

callStackPush :: Int -> Machine -> Machine
callStackPush c (Machine pc st hp cs ls env) =
    Machine pc st hp ((c, ls):cs) newLocalStack env

callStackPop :: Machine -> (Int, Machine)
callStackPop (Machine pc st hp ((c,ls):cs) _ env) =
    (c,Machine pc st hp cs ls env)

incrPC :: Machine -> Machine
incrPC (Machine pc st hp cs ls env) =
    Machine (pc + 1) st hp cs ls env

mapStack :: (Stack -> Stack) -> Machine -> Machine
f `mapStack` (Machine pc st hp cs ls env) =
    Machine pc (f st) hp cs ls env

mapHeap :: (Heap -> Heap) -> Machine -> Machine
f `mapHeap` (Machine pc st hp cs ls env) =
    Machine pc st (f hp) cs ls env

mapLocal :: (LocalStack -> LocalStack) -> Machine -> Machine
f `mapLocal` (Machine pc st hp cs ls env) =
    Machine pc st hp cs (f ls) env

mapLabels :: (Env -> Env) -> Machine -> Machine
f `mapLabels` (Machine pc st hp cs ls env) =
    Machine pc st hp cs ls (f env)

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
execute :: Asm -> Machine -> Machine
execute i@(Jump _) m = execute' i m
execute i@(JumpIf _) m = execute' i m
execute i@(Call _) m = execute' i m
execute (Ret) m = execute' Ret m
execute i m = incrPC $ execute' i m

setLabel :: Asm -> Machine -> Machine
setLabel (Label l) m = incrPC $ const (envSet l c ls) `mapLabels` m
    where c = machinePC m
          ls = machineLabels m
setLabel _ m = incrPC m

execute' :: Asm -> Machine -> Machine
execute' (Add) m = stackBinOp (+) `mapStack` m
execute' (Sub) m = stackBinOp (-) `mapStack` m
execute' (Mul) m = stackBinOp (*) `mapStack` m
execute' (Lt) m = stackBinOp (boolToValue <.> (<)) `mapStack` m
execute' (Le) m = stackBinOp (boolToValue <.> (<=)) `mapStack` m
execute' (Gt) m = stackBinOp (boolToValue <.> (>)) `mapStack` m
execute' (Ge) m = stackBinOp (boolToValue <.> (>=)) `mapStack` m
execute' (Eq) m = stackBinOp (boolToValue <.> (==)) `mapStack` m
execute' (Not) m = stackApp (\v -> case v of
                                       0 -> 1
                                       _ -> 0) `mapStack` m
execute' (StoreI loc) m = heapSet loc (stackPeek $ machineStack m) `mapHeap` m
execute' (LoadI loc) m = stackPush (heapGet loc $ machineHeap m) `mapStack` m
execute' (Store) m = heapSet (fromIntegral loc) val `mapHeap` m
    where (loc, val) = (\(Stack (x:y:_)) -> (x, y)) $ machineStack m
execute' (Load) m = stackPush (heapGet loc' $ machineHeap m) `mapStack` m
    where loc = stackPeek $ machineStack m
          loc' = fromIntegral loc
execute' (Push v) m = stackPush v `mapStack` m
execute' (Pop) m = stackPop `mapStack` m
execute' (Dup) m = stackPush (stackPeek $ machineStack m) `mapStack` m
execute' (Swap) m = stackSwap `mapStack` m
execute' (PushLocal) m = stackPop `mapStack` (localPush
                                              (stackPeek $ machineStack m)
                                                 `mapLocal` m)
execute' (PopLocal) m = const lds `mapLocal` (stackPush v `mapStack` m)
    where (v, lds) = localPop $ machineLocalStack m
execute' (Label _) m = m
execute' (Jump n) m = setCounter (envGet n $ machineLabels m) m
execute' (JumpIf n) m
    | stackPeek (machineStack m) == 0 = incrPC popped
    | otherwise = setCounter (envGet n $ machineLabels m) popped
    where popped = stackPop `mapStack` m
execute' (Call n) m = setCounter target . callStackPush current $ m
    where current = machinePC m
          target = envGet n $ machineLabels m
execute' (Ret) m = setCounter (callPoint + 1) m'
    where (callPoint, m') = callStackPop m

boolToValue :: Bool -> Value
boolToValue False = 0
boolToValue True = 1

(<.>) :: (a -> Value) -> (Value -> Value -> a) -> Value -> Value -> Value
(<.>) f op v w = f $ op v w

-- | Run a program on a brand new machine
run :: [Asm] -> Machine
run is = run' (V.fromList $ execute <$> is)
    (setMain . setCounter 0 . prepare is $ newMachine 65536)

setMain :: Machine -> Machine
setMain m = setCounter _main m
    where _main = envGet "main" $ machineLabels m

run' :: Vector (Machine -> Machine) -> Machine -> Machine
run' is m
    | end pc = m
    | otherwise = run' is (is ! pc $ m)
    where pc = machinePC $ m
          end = ((V.length is) ==)

prepare :: [Asm] -> Machine -> Machine
prepare is = appEndo . getDual $ foldMap (Dual . Endo . setLabel) is

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
test4 :: [Asm]
test4 = [ Label "fib"     -- n
        , PushLocal
        , Push 0
        , Push 1
        , PopLocal
        , Call "fib_rec"
        , Pop
        , Pop
        , Ret
        , Label "fib_rec"
        , Dup
        , Push 0
        , Eq
        , JumpIf "fib_done"
        , PushLocal
        , Dup
        , PushLocal
        , Add
        , PopLocal
        , Swap
        , Push 1
        , PopLocal
        , Sub
        , Jump "fib_rec"
        , Label "fib_done"
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
