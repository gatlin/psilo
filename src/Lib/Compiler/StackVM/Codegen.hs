{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler.StackVM.Codegen where

import Lib.Util
import Lib.Syntax
import Lib.Types.Scheme
import Lib.Types.Qual
import Lib.Types.Type
import Lib.Compiler.StackVM.Machine
import Data.Word
import Control.Monad.Free
import Data.Maybe (fromJust)
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Comonad
import Control.Comonad.Cofree

asm_ops :: Symbol -> Maybe Asm
asm_ops "add16" = Just Add16
asm_ops "mul16" = Just Mul16
asm_ops "modulo16" = Just Mod16
asm_ops "add32" = Just Add32
asm_ops "mul32" = Just Mul32
asm_ops "modulo32" = Just Mod32
asm_ops "lt" = Just Lt
asm_ops "lte" = Just Le
asm_ops "gt" = Just Gt
asm_ops "gte" = Just Ge
asm_ops "eq" = Just Eq
asm_ops "not" = Just Not
asm_ops "&" = Just And
asm_ops "|" = Just Or
asm_ops "^" = Just Xor
asm_ops "comp" = Just Comp
asm_ops ">>" = Just ShiftR
asm_ops "<<" = Just ShiftL
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

codegen :: MonadIO m => AnnotatedExpr Scheme -> CodegenT m [Asm]
codegen expr = go expr where
    go (scheme :< (FloatC n)) = return [ Push $ fromInteger $ round n ]
    go (scheme :< (IntC n)) = return [ Push $ fromInteger n ]
    go (scheme :< (BoolC b)) = return [ Push $ if b then 0x1 else 0x0 ]

    go ((Forall _ (_ :=> ty)) :< (IdC s)) = do
        --liftIO . putStrLn $ s ++  " : " ++ (show ty)
        env <- asks symbolEnv
        case safeEnvGet s env of
            Just loc -> case ty of
                TFun _ -> return [ ReadLocal loc, CallA ]
                _    -> return [ ReadLocal loc ]
            Nothing -> case asm_ops s of
                Just asm -> return [ asm ]
                Nothing -> return [ Call s ]

    go (scheme :< (AppC op erands)) = do
        erands' <- push_on_stack (reverse erands)
        op' <- go op
        return $ erands' ++ op'

    go (scheme :< (FunC args body)) = do
        let numbered_args = zip args [0..]
        body' <- local (store_args numbered_args) $ do
            b <- go body
            case b of
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

    go (scheme :< (IfC c t e)) = do
        c' <- go c
        t' <- go t
        e' <- go e
        gs <- gensym
        let if_label = "if_" ++ (show gs)
        return $ c' ++ [ JumpIf if_label ] ++
            e' ++ [ Ret,  Label if_label ] ++ t'

    go _ = return []

    push_on_stack :: MonadIO m => [ AnnotatedExpr Scheme ] -> CodegenT m [ Asm ]
    push_on_stack exprs = do
        exprs' <- forM exprs go
        return $ concat exprs'

    store_args :: [(Symbol, Location)] -> CodegenContext -> CodegenContext
    store_args syms cc = cc { symbolEnv = se, heapBase = hb }
        where se = (envFrom syms) <> (symbolEnv cc)
              hb = (heapBase cc) + (length syms)
