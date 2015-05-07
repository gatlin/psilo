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

type Location = Int

type Environment = [(Symbol, Value)]
type Store = IntMap.IntMap Value

instance Show Store where
    show str = "Amount in store: " ++ (show (IntMap.size str))

data MachineState = MachineState
    { mSto    :: Store       -- ^ the persistent store
    , mLoc    :: Location    -- ^ pointer to the store
    , mGlo    :: Environment -- ^ global environment
    } deriving (Show)

instance Monoid MachineState where
    mempty = MachineState { mSto = IntMap.empty
                          , mLoc = 0
                          , mGlo = [] }
    (MachineState s l g) `mappend` (MachineState s' l' g') =
        MachineState (IntMap.union s s') (l + l') (g <> g')

newMachineState :: MachineState
newMachineState = MachineState IntMap.empty 0 []

newtype Machine a = M {
    runM :: WriterT [String] (ReaderT Environment (StateT MachineState IO)) a
} deriving (Monad, MonadIO, MonadState MachineState, MonadReader Environment
           , MonadWriter [String], Functor, Applicative)

runMachine :: MachineState -> Machine a -> IO ((a, [String]), MachineState)
runMachine st k = runStateT (runReaderT (runWriterT (runM k)) []) st

data Value
    = VNil
    | VInteger Integer
    | VBoolean Bool
    | VPointer Location
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
                            ", body = " ++ (show b) ++
                            ", captured = " ++ (show e) ++ " } "
    show (VPointer p) = "<pointer:" ++ (show p) ++ ">"

log msg = tell [msg] -- provided by @WriterT@

query :: Symbol -> Machine (Maybe Value)
query sym = do
    env <- ask
    glo <- gets mGlo
    return $ lookup sym (env ++ glo)

store :: Location -> Value -> Machine ()
store loc val = do
    sto <- gets mSto
    modify $ \st -> st { mSto = IntMap.insert loc val sto }

fetch :: Location -> Machine Value
fetch loc = do
    sto <- gets mSto
    let mv = IntMap.lookup loc sto
    case mv of
        Just v -> return v
        _      -> do
            log $ "wtf"
            return VNil

fresh :: Machine Location
fresh = do
    loc <- gets mLoc
    modify $ \st -> st { mLoc = loc + 1 }
    return loc

dealloc :: Value -> Machine ()
dealloc (VPointer p) = do
    sto <- gets mSto
    v <- fetch p
    case v of
        (VClosure a b []) -> return ()
        (VClosure a b _ ) -> do
            modify $ \st -> st { mSto = IntMap.delete p sto }
        _ -> return ()
dealloc _ = return ()

eval :: Expr () -> Machine Value
eval (Free (AInteger n)) = return $ VInteger n
eval (Free (ABoolean b)) = return $ VBoolean b

eval (Free (ASymbol sym)) = do
    maybeVal <- query sym
    case maybeVal of
        Just val -> strict val >>= return
        Nothing  -> error $ "No value for symbol " ++ sym

eval (Free (ALambda (Free (AList args)) body)) = do
    env <- ask
    glo <- gets mGlo
    args' <- forM args $ \(Free (ASymbol arg)) -> return arg
    let vars = (map (\(Free (ASymbol sym)) -> sym) $ freeVariables body) \\ (map fst glo)
    log $ "Free variables captured: " ++ (show vars)
    let env' = filter (\(sym, val) -> elem sym vars) env
    let clos = VClosure args' body $ nub env'
    loc <- fresh
    store loc clos
    log $ "Creating closure"
    return $ VPointer loc

eval (Free (AApply op (Free (AList erands)))) = case builtin op of
    Just op' -> op' erands
    Nothing -> do
        fValue <- eval op >>= strict
        go fValue
    where go fValue@(VClosure syms body cl) = do
              log $ "Applying op: " ++ (show fValue)
              let diff = (length syms) - (length erands)
              env <- ask
              erands' <- forM erands $ \e -> do
                  loc <- fresh
                  store loc $ VThunk e env
                  return $ VPointer loc
              if (diff > 0)
                  then return $
                      VClosure (drop diff syms) body $
                        (zip (take diff syms) erands') ++ cl
                  else do
                      let args = zip syms erands'
                      result <- local ((args ++ cl) ++) $ eval body
                      -- forM_ (args ++ cl) $ \(sym, val) -> dealloc val
                      return result

          go v = return v

eval (Free (ADefine sym body)) = do
    val <- eval body
    loc <- fresh
    glo <- gets mGlo
    modify $ \st -> st { mGlo = (sym, val):glo }
    return VNil

eval (Pure _) = return VNil

strict :: Value -> Machine Value
strict (VThunk body cl) = do
    result <- local (cl ++) $ eval body
    return result
strict ptr@(VPointer loc) = do
    v <- fetch loc
    dealloc ptr
    strict v
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

freeVariables :: Expr () -> [Expr ()]
freeVariables (Free (ALambda (Free (AList args)) body)) = (freeVariables body) \\ args
freeVariables (Free (AApply op (Free (AList erands)))) =
    (freeVariables op) ++ (concatMap freeVariables erands)
freeVariables s@(Free (ASymbol _)) = [s]
freeVariables _ = []
