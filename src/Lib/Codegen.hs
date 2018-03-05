{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Codegen where

import Data.Semigroup ((<>))

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Applicative

import Control.Comonad.Cofree

import LLVM.Module
import LLVM.Context
import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Float as F

import Data.ByteString.Short (ShortByteString)

import Lib.Syntax ( Symbol
                  , AnnotatedExpr(..)
                  , TopLevel(..)
                  , CoreAst(..)
                  , LiftedExpr(..)
                  , liftExpr)

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(ShortByteString, Operand)]

data CodegenState = CodegenState
    { currentBlock :: Name
    , blocks :: Map.Map Name BlockState
    , symtab :: SymbolTable
    , blockCount :: Int
    , count :: Word
    , names :: Names
    } deriving ( Show )

data BlockState = BlockState
    { idx :: Int
    , stack :: [ Named Instruction ]
    , term :: Maybe (Named Terminator)
    } deriving ( Show )

newtype Codegen a = Codegen {
    runCodegen :: State CodegenState a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState CodegenState
               )

newtype LLVM a = LLVM (State AST.Module a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState AST.Module
             )

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

entryBlockName :: ShortByteString
entryBlockName = "entry"

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: T.Type -> ShortByteString -> [(T.Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults
    { name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = body
    }

external :: T.Type -> ShortByteString -> [(T.Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults
    { name        = Name label
    , linkage     = L.External
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }

entry :: Codegen Name
entry = gets currentBlock

addBlock :: ShortByteString -> Codegen Name
addBlock bname = do
    bls <- gets blocks
    ix <- gets blockCount
    nms <- gets names

    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms

    modify $ \s -> s {
        blocks = Map.insert (Name qname) new bls,
        blockCount = ix + 1,
        names = supply }

    return (Name qname)

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

setBlock :: Name -> Codegen Name
setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ (show c)

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + 1 }
    return $ i + 1

type Names = Map.Map ShortByteString Int

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
    case Map.lookup nm ns of
        Nothing -> (nm, Map.insert nm 1 ns)
        Just ix -> (nm <> (fromString $ show ix), Map.insert nm (ix + 1) ns)

local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: ShortByteString -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symtab
    modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: ShortByteString -> Codegen Operand
getvar var = do
    syms <- gets symtab
    case lookup var syms of
        Just x -> return x
        Nothing -> error $ "Local variable not in scope: " ++ (show var)

instr :: Instruction -> Codegen (Operand)
instr ins = do
    n <- fresh
    let ref = (UnName n)
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = (ref := ins) : i } )
    return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
    blk <- current
    modifyBlock (blk { term = Just trm })
    return trm

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

cons :: C.Constant -> Operand
cons = ConstantOperand

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegenTop :: LiftedExpr -> LLVM ()
codegenTop (FunL name args body) = define double (fromString name) fnargs bls
    where
        fnargs = toSig args
        bls = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \a -> do
                var <- alloca double
                store var (local (AST.Name (fromString a)))
                assign (fromString a) var
            cgen body >>= ret

codegenTop exp = define double "main" [] blks
    where
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            cgen exp >>= ret

toSig :: [Symbol] -> [(AST.Type, AST.Name)]
toSig = fmap (\x -> (double, AST.Name (fromString x)))

binops = Map.fromList
    [ ("+", fadd)
    , ("*", fmul)
    , ("-", fsub)
    , ("/", fdiv)
    ]

cgen :: LiftedExpr -> Codegen AST.Operand
cgen (FloatL n) = return $ cons $ C.Float (F.Double n)
cgen (IdL s) = getvar (fromString s) >>= load
cgen (AppL op erands) = do
    largs <- mapM cgen erands
    case Map.lookup op binops of
        Nothing ->
            call (externf (AST.Name (fromString op))) largs
        Just binop -> do
            binop (largs !! 0) (largs !! 1)

cgen _ = error "You goofed"

codegen :: AST.Module -> [LiftedExpr] -> IO AST.Module
codegen mod fns = do
    withContext $ \context -> do
        withModuleFromAST context newast $ \m -> do
            llstr <- moduleLLVMAssembly m
            putStrLn . show $ llstr
            return newast

    where
        modn = mapM codegenTop fns
        newast = runLLVM mod modn
