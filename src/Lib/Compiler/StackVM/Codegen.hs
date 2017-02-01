module Lib.Compiler.StackVM.Codegen where

import Lib.Util
import Lib.Syntax
import Lib.Compiler.StackVM.Machine
import Data.Word
import Control.Monad.Free
import Data.Maybe (fromJust)
import Data.Monoid
import Control.Monad
import Control.Monad.Reader

asm_ops :: Symbol -> Maybe Asm
asm_ops "+" = Just Add
asm_ops "*" = Just Mul
asm_ops "-" = Just Sub
asm_ops "/" = Just Div
asm_ops "%" = Just Mod
asm_ops "<" = Just Lt
asm_ops "<=" = Just Le
asm_ops ">" = Just Gt
asm_ops ">=" = Just Ge
asm_ops "=" = Just Eq
asm_ops "not" = Just Not
asm_ops _ = Nothing

data CodegenContext = CodegenContext
    { symbolEnv :: Env
    , heapBase :: Location
    }

newCodegenContext :: CodegenContext
newCodegenContext = CodegenContext {
    symbolEnv = mempty,
    heapBase = 0
    }

codegen :: CoreExpr () -> [Asm]
codegen expr = optimize $ runReader (go expr) newCodegenContext where

    go :: CoreExpr () -> Reader CodegenContext [Asm]
    go (Free (IntC n)) = return [ Push (fromInteger n) ]
    go (Free (BoolC b)) = return [ Push $ if b then 0x1 else 0x0 ]

    go (Free (IdC s)) = do
        env <- asks symbolEnv
        hb <- asks heapBase
        case safeEnvGet s env of
            Just loc -> return [ LoadI loc ]
            Nothing  -> error $ "sym = " ++ s ++ ", Env = " ++ show env ++
                ", heap pointer = " ++ show hb

    go (Free (DefC sym val)) = do
        body <- go val
        return $ (Label sym) : body

    go (Free (AppC fun operands)) = case fun of
        (Free (IdC sym)) -> do
            operands' <- push_on_stack operands
            case asm_ops sym of
                Just op -> return $ operands' ++ [ op ]
                Nothing -> return $ operands' ++ [ Call sym ]

    go (Free (ClosC args body)) = do
        hb <- asks heapBase
        let numbered_args = zip args [hb..]
        body' <- local (store_args numbered_args) $ go body
        args' <- forM (fmap snd (reverse numbered_args)) $ \loc -> return
            [ StoreI loc, Pop ]
        return $ (concat args') ++ body' ++ [ Ret ]

    go (Free (IfC c t e)) = do
        c' <- go c
        t' <- go t
        e' <- go e
        return $ c' ++ [ JumpIf "if_then" ] ++
            e' ++ [ Ret,  Label "if_then" ] ++ t'

    store_args :: [(Symbol, Location)] -> CodegenContext -> CodegenContext
    store_args syms cc = cc { symbolEnv = se, heapBase = hb }
        where se = (envFrom syms) <> (symbolEnv cc)
              hb = (heapBase cc) + (length syms)

    push_on_stack :: [ CoreExpr () ] -> Reader CodegenContext [ Asm ]
    push_on_stack exprs = do
        exprs' <- forM exprs go
        return $ concat exprs'
{-
    go (Free (ClosC args body)) = local extendEnv $ args' ++ body'
        where numbered_args :: [(Symbol, Location)]
              numbered_args = zip args [0..]
-}

optimize :: [ Asm ] -> [ Asm ]
optimize = id
