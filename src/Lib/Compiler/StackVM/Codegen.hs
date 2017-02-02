{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.State
import Control.Monad.IO.Class

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
    } deriving (Show)

newCodegenContext :: CodegenContext
newCodegenContext = CodegenContext {
    symbolEnv = mempty,
    heapBase = 0
    }

data CodegenState = CodegenState
    { gensymValue :: Int
    , currTopLevel :: Maybe Symbol
    } deriving (Show)

newCodegenState :: CodegenState
newCodegenState = CodegenState {
    gensymValue = 0,
    currTopLevel = Nothing
    }

newtype CodegenT m a = CodegenT {
    unCodegenT :: StateT CodegenState (ReaderT CodegenContext m) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState CodegenState
               , MonadReader CodegenContext
               , MonadIO
               )

runCodegenT
    :: Monad m
    => CodegenContext
    -> CodegenState
    -> CodegenT m a
    -> m a
runCodegenT ctx st c = runReaderT (evalStateT (unCodegenT c) st) ctx

gensym :: Monad m => CodegenT m Int
gensym = do
    gs <- gets gensymValue
    modify $ \cc -> cc { gensymValue = gs + 1 }
    return gs

codegen :: MonadIO m => CoreExpr () -> m [Asm]
codegen expr = runCodegenT newCodegenContext newCodegenState (go expr) where

    go :: MonadIO m => CoreExpr () -> CodegenT m [ Asm ]
    go (Free (IntC n)) = return [ Push (fromInteger n) ]
    go (Free (BoolC b)) = return [ Push $ if b then 0x1 else 0x0 ]

    go (Free (IdC s)) = do
        env <- asks symbolEnv
        hb <- asks heapBase
        case safeEnvGet s env of
            Just loc -> return [ ReadLocal loc ]
            Nothing  -> error $ "sym = " ++ s ++ ", Env = " ++ show env ++
                ", heap pointer = " ++ show hb

    go (Free (DefC sym val)) = do
        modify $ \st -> st { currTopLevel = Just sym }
        body <- go val
        return $ (Label sym) : body

    go (Free (AppC fun operands)) = do
        operands' <- push_on_stack operands
        case fun of
            (Free (IdC sym)) -> case asm_ops sym of
                Just op -> return $ operands' ++ [ op ]
                Nothing -> return $ operands' ++ [ Call sym ]

    go (Free (TailRecC operands)) = do
        operands' <- push_on_stack operands
        mTld <- gets currTopLevel
        case mTld of
            Nothing -> error $ "Cannot make tail-recursive call"
            Just tld -> return $ operands' ++ [ Jump tld ]

    go (Free (ClosC args body)) = do
        hb <- asks heapBase
        let numbered_args = zip (reverse args) [0..]
        body' <- local (store_args numbered_args) $ do
            b <- go body
            let b' = reverse b
            case reverse b' of
                (Call sym):rest -> do
                    mTld <- gets currTopLevel
                    case fmap (== sym) mTld of
                        Just True -> return $ reverse $ Jump sym : rest
                        _ -> return b
                _ -> return b
        args' <- forM (fmap snd numbered_args) $ \loc -> return
            [ WriteLocal loc ]
        -- for tail call elimination: is the last thing in body' a Call?
        return $ (concat args') ++ body' ++ [ Ret ]

    go (Free (IfC c t e)) = do
        c' <- go c
        t' <- go t
        e' <- go e
        gs <- gensym
        let if_label = "if_" ++ (show gs)
        return $ c' ++ [ JumpIf if_label ] ++
            e' ++ [ Ret,  Label if_label ] ++ t'

    store_args :: [(Symbol, Location)] -> CodegenContext -> CodegenContext
    store_args syms cc = cc { symbolEnv = se, heapBase = hb }
        where se = (envFrom syms) <> (symbolEnv cc)
              hb = (heapBase cc) + (length syms)

    push_on_stack :: MonadIO m => [ CoreExpr () ] -> CodegenT m [ Asm ]
    push_on_stack exprs = do
        exprs' <- forM exprs go
        return $ concat exprs'
