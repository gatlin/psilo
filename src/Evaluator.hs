{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator

where

import Control.Monad.Free
import Prelude hiding (log)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Free
import Control.Monad.Trans
import qualified Data.IntMap.Strict as IntMap
import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, sequence)
import Data.List (intersperse, nub, (\\), concat, intersect)
import Data.Monoid
import Control.Applicative

import Parser
import Syntax

data Value
    = VNil
    | VInteger Integer
    | VBoolean Bool
    | VSymbol  Symbol
    | VClosure { clArgs :: [Symbol]
               , clBody :: (Expr ())
               , clEnv  :: Environment }
    | VThunk (Expr ()) Environment -- ^ Suspended computation
    deriving (Eq)

instance Show Value where
    show VNil = "()"
    show (VBoolean b) = show b
    show (VInteger n) = show n
    show (VSymbol  s) = '\'':s
    show (VThunk   e c) = "<thunk> { " ++ (show e) ++ " } "
    show (VClosure a b e) = "<closure> { args = " ++ (show a) ++
                            ", body = " ++ (show b) ++ " } "

type Location = Int

type Environment = [(Symbol, Location)]
type Store = IntMap.IntMap Value

data MachineState = MachineState
    { mSto    :: Store
    , mLoc    :: Location
    } deriving (Show)

newMachineState = MachineState IntMap.empty 0

newtype Machine a = M {
    runM :: WriterT [String] (ReaderT Environment (StateT MachineState IO)) a
} deriving (Monad, MonadIO, MonadState MachineState, MonadReader Environment
           , MonadWriter [String], Functor, Applicative)

runMachine :: MachineState -> Environment -> Machine a -> IO ((a, [String]), MachineState)
runMachine st ev k = runStateT (runReaderT (runWriterT (runM k)) ev) st

log msg = tell [msg] -- provided by @WriterT@

eval :: Expr () -> Machine Value
eval _ = return VNil

strict :: Value -> Machine Value
strict (VThunk body cl) = do
    result <- local (const cl) $ eval body
    return result
strict v = return v

builtin (Free (ASymbol sym)) = case sym of
    "+" -> Just $ \operands -> do
        operands' <- cleanse operands
        return $ VInteger $ sum operands'
    "*" -> Just $ \operands -> do
        operands' <- cleanse operands
        return $ VInteger $ product operands'
    "-" -> Just $ \operands -> do
        operands' <- cleanse operands
        return $ VInteger $ (operands' !! 0) - (operands' !! 1)
    "/" -> Just $ \operands -> do
        operands' <- cleanse operands
        return $ VInteger $ (operands' !! 0) `div` (operands' !! 1)
    "=?" -> Just $ \operands -> do
        operands' <- cleanse operands
        return $ VBoolean $ (operands' !! 0) == (operands' !! 1)
    "mod" -> Just $ \operands -> do
        operands' <- cleanse operands
        return $ VInteger $ (operands' !! 0) `mod` (operands' !! 1)
    "and" -> Just $ \operands -> do
        arg1 <- eval (operands !! 0) >>= strict
        if arg1 == (VBoolean False)
            then return (VBoolean False)
            else do
                arg2 <- eval (operands !! 1) >>= strict
                if arg2 == (VBoolean False)
                    then return (VBoolean False) else return $ VBoolean True
    "or" -> Just $ \operands -> do
        arg1 <- eval (operands !! 0) >>= strict
        if arg1 == (VBoolean True)
            then return arg1
            else do
                arg2 <- eval (operands !! 1) >>= strict
                if arg2 == (VBoolean True)
                    then return arg2 else return $ VBoolean False
    "if" -> Just $ \operands -> do
        VBoolean c <- eval (operands !! 0) >>= strict
        if c then eval (operands !! 1) >>= strict
             else eval (operands !! 2) >>= strict
    _ -> Nothing
    where cleanse xs = forM xs $ \o -> do
              s <- eval o >>= strict
              case s of
                  VInteger n -> return n
                  other -> do
                      log $ "wtf, got " ++ (show other)
                      return 0
builtin _ = Nothing

