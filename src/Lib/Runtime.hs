{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Runtime where

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Lib.Syntax

type Location = Int

data Value
    = NumV { numV :: Double }
    | BoolV { boolV :: Bool }
    | StringV { stringV :: String }
    | SymV { symV :: Symbol }
    | ClosV { closVArgs :: [Symbol], closVBody :: Expr (), closVEnv :: Env }
    deriving (Show)

-- * Environment

data Binding = Binding
    { bindingSym :: Symbol
    , bindingLoc :: Location
    } deriving (Show)

type Env = [Binding]

bind :: Symbol -> Location -> Binding
bind = Binding

emptyEnv :: Env
emptyEnv = []

envIsEmpty :: Env -> Bool
envIsEmpty env = (length env) == 0

extendEnv :: Binding -> Env -> Env
extendEnv = (:)

extendEnv' :: Env -> Env -> Env
extendEnv' = (++)

lookup' :: Symbol -> Env -> Maybe Location
lookup' _ [] = Nothing
lookup' which ((Binding sym loc):rest)
    | sym == which = Just loc
    | otherwise = lookup' which rest

-- * Storage

newtype Store = Store { unStore :: (IntMap Value) }
    deriving (Show)

emptyStore :: Store
emptyStore = Store $ IM.empty

storeIsEmpty :: Store -> Bool
storeIsEmpty (Store stor) = IM.null stor

-- * Runtime

data RuntimeState = RuntimeState
    { storeLoc :: Location
    , storage :: Store
    } deriving (Show)

newtype Runtime a = Runtime {
    unRuntime :: ReaderT Env (StateT RuntimeState IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader Env
               , MonadState RuntimeState )

type Result = (Value, RuntimeState)

doRuntime :: Runtime a -> IO (a, RuntimeState)
doRuntime rt = runStateT (runReaderT (unRuntime rt) emptyEnv) $
    RuntimeState 0 emptyStore

nextLoc :: Runtime Location
nextLoc = do
    currentLoc <- gets storeLoc
    modify $ \st -> st { storeLoc = currentLoc + 1 }
    return currentLoc

getEnv :: Runtime Env
getEnv = ask


lookup :: Symbol -> Runtime (Maybe Location)
lookup sym = do
    env <- getEnv
    return $ lookup' sym env

overrideStore :: Location -> Value -> Runtime ()
overrideStore loc val = do
    Store theStorage <- gets storage
    modify $ \st -> st { storage = Store $ IM.insert loc val theStorage }

removeFromStore :: Location -> Runtime ()
removeFromStore loc = do
    Store theStorage <- gets storage
    modify $ \st -> st { storage = Store $ IM.delete loc theStorage }

duplicateInStore :: Location -> Location -> Runtime Value
duplicateInStore aLoc bLoc = do
    Just val <- fetch aLoc
    overrideStore bLoc val
    return val

valIsClos :: Value -> Bool
valIsClos (ClosV _ _ _) = True
valIsClos _             = False

fetch :: Location -> Runtime (Maybe Value)
fetch loc = do
    Store stor <- gets storage
    case IM.member loc stor of
        False -> return Nothing
        True -> do
            let val = stor IM.! loc
            when (valIsClos val) $ do
                modify $ \st -> st {
                    storage = Store $  case envIsEmpty (closVEnv val) of
                            True -> stor
                            False -> IM.delete loc stor
                    }
            return $ Just val

lookupAndFetch :: Symbol -> Runtime (Maybe Value)
lookupAndFetch sym = do
    mLoc <- lookup sym
    case mLoc of
        Nothing -> return Nothing
        Just loc -> fetch loc
