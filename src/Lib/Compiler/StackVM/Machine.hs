{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
This began as and is heavily inspired by

    https://github.com/taiki45/hs-vm/

However,

* Some terminology has changed
* The local data stack is more like a set of registers
* The ISA has been expanded
* The 'MachineT' type is a monad transformer
-}

module Lib.Compiler.StackVM.Machine
    ( MachineT(..)
    , runMachine
    , Asm(..)
    , Location
    , Value
    , Env(..)
    , envGet
    , safeEnvGet
    , envSet
    , envFrom
    , run
    , execute
    , stackPeek
    , MachineState(..)
    )
where

import Control.Applicative
import Data.Functor.Identity
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromJust)
import Data.Word
import Data.Bits

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State

import Lib.Syntax

type Location = Int

-- | This VM has 16-bit words.
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

envFrom :: [(Symbol, Location)] -> Env
envFrom symlocs = Env $ M.fromList symlocs

safeEnvGet :: Symbol -> Env -> Maybe Location
safeEnvGet sym (Env env) = M.lookup sym env

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

-- | Local Data
newtype LocalData = LocalData {
    unLocalData :: Vector Value
    } deriving (Show)

localRead :: LocalData -> Int -> Value
localRead (LocalData vs) idx = vs ! idx

localWrite :: Int -> Value -> LocalData -> LocalData
localWrite idx val (LocalData vs) = LocalData $ vs V.// [(idx, val)]

newLocalData :: LocalData
newLocalData = LocalData $ V.replicate 16 0

-- | A vector-based heap implementation
newtype Memory = Memory {
    unMemory :: Vector Value
    }

instance Show Memory where
    show (Memory mem) = "Memory<" ++ (show $ V.length mem) ++ ">"

memorySet :: Location -> Value -> Memory -> Memory
memorySet loc val (Memory mem) = Memory $ mem V.// [(loc, val)]

memoryGet :: Location -> Memory -> Value
memoryGet loc (Memory mem) = mem ! loc

newMemory :: Int {- ^ Size -} -> Memory
newMemory sz = Memory $ V.replicate sz 0

-- | Call stack. Not a newtype because ultimately the machine is the wrapper
-- around this structure.
type CallStack = [(Location, LocalData)]

newCallStack :: CallStack
newCallStack = [(0, newLocalData)]

-- | The actual machine definition
data MachineState = M
    { machinePC :: Location
    , machineStack :: Stack
    , machineMemory :: Memory
    , machineCallStack :: CallStack
    , machineLocalData :: LocalData
    , machineLabels :: Env
    } deriving (Show)

newtype MachineT m a = Machine {
    unMachineT :: StateT MachineState m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState MachineState
               , MonadIO
               , MonadTrans )

type Machine = MachineT IO

runMachine :: Monad m => MachineState -> MachineT m a -> m (a, MachineState)
runMachine ms m = runStateT (unMachineT m) ms

newMachineState :: Int {- ^ Memory size -} -> MachineState
newMachineState memorySz = M
    0
    newStack
    (newMemory memorySz)
    newCallStack
    newLocalData
    mempty

setCounter :: Int -> MachineState -> MachineState
setCounter c (M _ st hp cs ls env) = M c st hp cs ls env

callStackPush :: Int -> MachineState -> MachineState
callStackPush c (M pc st hp cs ls env) =
    M pc st hp ((c, ls):cs) newLocalData env

callStackPop :: MachineState -> (Int, MachineState)
callStackPop (M pc st hp ((c,ls):cs) _ env) =
    (c,M pc st hp cs ls env)

incrPC :: MachineState -> MachineState
incrPC (M pc st hp cs ls env) =
    M (pc + 1) st hp cs ls env

mapStack :: (Stack -> Stack) -> MachineState -> MachineState
f `mapStack` (M pc st hp cs ls env) =
    M pc (f st) hp cs ls env

mapMemory :: (Memory -> Memory) -> MachineState -> MachineState
f `mapMemory` (M pc st hp cs ls env) =
    M pc st (f hp) cs ls env

mapLocal :: (LocalData -> LocalData) -> MachineState -> MachineState
f `mapLocal` (M pc st hp cs ls env) =
    M pc st hp cs (f ls) env

mapLabels :: (Env -> Env) -> MachineState -> MachineState
f `mapLabels` (M pc st hp cs ls env) =
    M pc st hp cs ls (f env)

-- * Low level instructions

-- | The assembly instructions for the stack machine
data Asm
    = Add16           -- ^ 16-bit addition
    | Mul16           -- ^ 16-bit multiplication
    | Mod16           -- ^ 16-bit modulus
    | Add32           -- ^ 32-bit addition
    | Mul32           -- ^ 32-bit multiplication
    | Mod32           -- ^ 32-bit modulus
    | Lt              -- ^ second stack value < first stack value
    | Le              -- ^ '' <= ''
    | Gt              -- ^ '' > ''
    | Ge              -- ^ '' >= ''
    | Eq              -- ^ '' == ''
    | Not             -- ^ Turnover bool: non-zero => 0, 0 => 1
    | And             -- ^ Bitwise AND of the top two stack values
    | Or              -- ^ Bitwise OR of the top two stack values
    | Xor             -- ^ Bitwise XOR of the top two stack values
    | Comp            -- ^ Bitwise complement of the top stack value
    | ShiftL          -- ^ Shift top stack value left by value under it
    | ShiftR          -- ^ Shift top stack value right by value under it
    | StoreI Location -- ^ Store top of stack to immediate location
    | LoadI Location  -- ^ Push value at immediate location onto stack
    | Store           -- ^ See [1] below this definition
    | Load            -- ^ See [1] below this definition
    | Push Value      -- ^ Push constant value to stack
    | Pop             -- ^ Pop and discard value from stack
    | Dup             -- ^ Duplicate top value on stack
    | Swap            -- ^ Swap the top two values on the stack
    | ReadLocal Int   -- ^ See [2] below this definition
    | WriteLocal Int  -- ^ See [2] below this definition
    | Label Symbol    -- ^ Set label and save current position
    | Jump Symbol     -- ^ Unconditional jump
    | JumpIf Symbol   -- ^ Jump if first stack value is non-zero. Then discard.
    | Call Symbol     -- ^ Store current PC and jump to function.
    | CallA           -- ^ Store PC and jump to function at address on stack
    | LoadA Symbol    -- ^ Push the address of the given symbol on the stack
    | Ret             -- ^ Return from calling point

{-
[1]: Like 'StoreI' and 'LoadI' but the locations are not hardcoded, but instead
the top stack value is the storage location, and the second is the actual value.

[2]: Read and write to the local "stack" (which needs to be renamed). "read"
pushes the value from the local onto the main stack while "write" pops the top
of the stack and inserts the value at the chosen index.
-}

instance Show Asm where
    show (Add16) = "add16"
    show (Mul16) = "mul16"
    show (Mod16) = "modulo16"
    show (Add32) = "add32"
    show (Mul32) = "mul32"
    show (Mod32) = "modulo32"
    show (Lt)  = "lt"
    show (Le)  = "lte"
    show (Gt)  = "gt"
    show (Ge)  = "gte"
    show (Eq)  = "eq"
    show (Not) = "not"
    show (And) = "and"
    show (Or)  = "or"
    show (Xor) = "xor"
    show (Comp) = "comp"
    show (ShiftL) = "shiftl"
    show (ShiftR) = "shiftr"
    show (StoreI loc) = "storei " ++ (show loc)
    show (LoadI loc) = "loadi " ++ (show loc)
    show (Store) = "store"
    show (Load) = "load"
    show (Push val) = "push " ++ (show val)
    show (Pop) = "pop"
    show (Dup) = "dup"
    show (Swap) = "swap"
    show (ReadLocal val) = "read " ++ (show val)
    show (WriteLocal val) = "write " ++ (show val)
    show (Label sym) = "label ." ++ sym
    show (Jump sym) = "jump ." ++ sym
    show (JumpIf sym) = "jumpif ." ++ sym
    show (Call sym) = "call ." ++ sym
    show (CallA) = "calla"
    show (LoadA sym) = "loada ." ++ sym
    show (Ret) = "return"

-- | Top level instruction execution.
execute :: Monad m => Asm -> MachineT m ()
execute i@(Jump _)= execute' i
execute i@(JumpIf _) = execute' i
execute i@(Call _) = execute' i
execute i@(CallA) = execute' i
execute (Ret) = execute' Ret
execute i = execute' i >> modify incrPC

setLabel :: Asm -> MachineState -> MachineState
setLabel (Label l) m = incrPC $ const (envSet l c ls) `mapLabels` m
    where c = machinePC m
          ls = machineLabels m
setLabel _ m = incrPC m

shiftL' a b = shiftL a (fromInteger . toInteger $ b)
shiftR' a b = shiftR a (fromInteger . toInteger $ b)

-- | Pop a 32-bit integer off a stack
pop32 :: Monad m => MachineT m [Value]
pop32 = do
    stack <- gets machineStack
    let result_be = stackPeek stack
    let stack' = stackPop stack
    let result_le = stackPeek stack'
    modify $ mapStack stackPop
    return [result_be, result_le]

binOp32 :: Monad m => (Word32 -> Word32 -> Word32) -> MachineT m ()
binOp32 op = do
    opA_s <- pop32 >>= return . map fromIntegral
    let opA = ((opA_s !! 0)`shiftL` 16) .|. (opA_s !! 1)
    opB_s <- pop32 >>= return . map fromIntegral
    let opB = ((opB_s !! 0)`shiftL` 16) .|. (opB_s !! 1)
    let result = op opA opB :: Word32
    let result_s = [result .&. 0xFF, (result .&. 0xFF00) `shiftR` 16]
    modify $ mapStack (stackPush $ fromIntegral $ result_s !! 0)
        . mapStack (stackPush $ fromIntegral $ result_s !! 1)

execute' :: Monad m => Asm -> MachineT m ()
execute' (Add16) = modify (mapStack (stackBinOp (+))) >> return ()
execute' (Mul16) = modify (mapStack (stackBinOp (*))) >> return ()
execute' (Mod16) = modify (mapStack (stackBinOp mod)) >> return ()

execute' (Add32) = binOp32 (+) >> return ()
execute' (Mul32) = binOp32 (*) >> return ()
execute' (Mod32) = binOp32 mod >> return ()

execute' (And) = modify (mapStack (stackBinOp (.&.))) >> return ()
execute' (Or)  = modify (mapStack (stackBinOp (.|.))) >> return ()
execute' (Xor) = modify (mapStack (stackBinOp xor)) >> return ()
execute' (ShiftL) = modify (mapStack (stackBinOp shiftL')) >> return ()
execute' (ShiftR) = modify (mapStack (stackBinOp shiftR')) >> return ()

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

execute' (Comp) = do
    modify $ mapStack (stackApp complement)
    return ()

execute' (StoreI loc) = do
    stack <- gets machineStack
    modify $ mapMemory (memorySet loc (stackPeek stack))

execute' (LoadI loc) = do
    memory <- gets machineMemory
    modify $ mapStack (stackPush (memoryGet loc memory))

execute' (Store) = do
    m <- get
    let (loc, val) = (\(Stack (x:y:_)) -> (x, y)) $ machineStack m
    put $ memorySet (fromIntegral loc) val `mapMemory` m

execute' (Load) = do
    stack <- gets machineStack
    memory <- gets machineMemory
    let loc = stackPeek stack
    let loc' = fromIntegral loc
    modify $ mapStack (stackPush (memoryGet loc' memory))
    return ()

execute' (Push v) = modify $ mapStack (stackPush v)
execute' (Pop) = modify $ mapStack stackPop
execute' (Dup) = do
    stack <- gets machineStack
    modify $ mapStack (stackPush (stackPeek stack))

execute' (Swap) = modify $ mapStack stackSwap
{-
execute' (PushLocal) = do
    stack <- gets machineStack
    modify $ mapStack stackPop .
        mapLocal (localPush (stackPeek stack))

execute' (PopLocal) = do
    m <- get
    let stack = machineStack m
    let local = machineLocalData m
    let (v, lds) = localPop local
    modify $ mapLocal (const lds) . mapStack (stackPush v)
-}
execute' (ReadLocal n) = do
    local <- gets machineLocalData
    let value = localRead local (fromIntegral n)
    modify $ mapStack (stackPush value)

execute' (WriteLocal n) = do
    local <- gets machineLocalData
    stack <- gets machineStack
    let local' = localWrite n (stackPeek stack) local
    modify $ mapStack stackPop . mapLocal (const local')
    {-
    LocalData local <- gets machineLocalData
    stack <- gets machineStack
    let (before, after) = splitAt n local
    let local' = LocalData $ before ++ [stackPeek stack] ++ (drop 1 after)
    modify $ mapStack stackPop . mapLocal (const local')-}

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

execute' (CallA) = do
    stack <- gets machineStack
    pc <- gets machinePC
    let target = fromIntegral $ stackPeek stack
    modify $ setCounter target . callStackPush pc . mapStack stackPop

execute' (LoadA sym) = do
    labels <- gets machineLabels
    modify $ mapStack (stackPush $ fromIntegral (envGet sym labels))

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

run :: Monad m => [Asm] -> m MachineState
run is = run' (V.fromList $ execute <$> is)
    (setMain . setCounter 0 . prepare is $ newMachineState 65536)

run'
    :: Monad m
    => Vector (MachineT m ())
    -> MachineState
    -> m MachineState
run' is ms
    | end pc = return ms
    | otherwise = do
          (_, ms') <- runMachine ms (is ! pc)
          run' is ms'
    where pc = machinePC ms
          end = ((V.length is) ==)
