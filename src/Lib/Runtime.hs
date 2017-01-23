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
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Lib.Syntax

type Location = Int

data Value
    = NumV { numV :: Double }
    | BoolV { boolV :: Bool }
    | StringV { stringV :: String }
    | SymV { symV :: Symbol }
    | ClosV { closVArgs :: [Symbol], closVBody :: CoreExpr (), closVEnv :: Env }
    | NopV

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = if b then "#t" else "#f"
    show (StringV s) = "\"" ++ s ++ "\""
    show (ClosV a b e) = "Closure<" ++ (show e) ++ ">"
    show _ = "Value"

-- * Environment

data Binding = Binding
    { bindingSym :: Symbol
    , bindingLoc :: Location
    } deriving (Show)

newtype Env = Env { unEnv :: Map Symbol Location } deriving (Show)

bind :: Symbol -> Location -> Binding
bind sym loc = Binding sym loc

emptyEnv :: Env
emptyEnv = Env M.empty

envIsEmpty :: Env -> Bool
envIsEmpty (Env env) = M.null env

envFromBindings :: [Binding] -> Env
envFromBindings bindings = go bindings emptyEnv where
    go [] env = env
    go ((Binding sym loc):rest) (Env env) = go rest $
        Env $ M.insert sym loc env

bindingsFromEnv :: Env -> [Binding]
bindingsFromEnv (Env env) = fmap (\(a,b) -> Binding a b) $ M.assocs env

extendEnv :: Binding -> Env -> Env
extendEnv (Binding sym loc) (Env env) = Env $ M.insert sym loc env

extendEnv' :: Env -> Env -> Env
extendEnv' (Env e1) (Env e2) = Env $ M.union e1 e2

lookup' :: Symbol -> Env -> Maybe Binding
lookup' sym (Env env)
    | M.null env = Nothing
    | otherwise = case M.lookup sym env of
          Nothing -> Nothing
          Just loc -> Just (Binding sym loc)

instance Monoid Env where
    mempty = emptyEnv
    mappend = extendEnv'

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
    , topLevelDefns :: Map Symbol Location
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

runRuntime :: RuntimeState -> Runtime a -> IO (a, RuntimeState)
runRuntime rs rt = runStateT (runReaderT (unRuntime rt) emptyEnv) rs

evalRuntime :: RuntimeState -> Runtime a -> IO a
evalRuntime rs rt = runRuntime rs rt >>= return . fst

execRuntime :: RuntimeState -> Runtime a -> IO RuntimeState
execRuntime rs rt = runRuntime rs rt >>= return . snd

defaultRuntimeState :: RuntimeState
defaultRuntimeState = RuntimeState {
    storeLoc = 0,
    storage = emptyStore,
    topLevelDefns = M.empty
    }

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
    return $ fmap bindingLoc $ lookup' sym env

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
        Nothing -> do
            tlds <- gets topLevelDefns
            let mLoc' = M.lookup sym tlds
            case mLoc' of
                Nothing -> error $ "Symbol " ++ sym ++ " not bound"
                Just loc -> fetch loc
        Just loc -> fetch loc

isSymTopLevel :: Symbol -> Runtime Bool
isSymTopLevel sym = do
    tlds <- gets topLevelDefns
    return $ M.member sym tlds

topLevelDefine :: Symbol -> CoreExpr () -> (CoreExpr () -> Runtime Value) -> Runtime Value
topLevelDefine sym expr k = do
    loc <- nextLoc
    tlds <- gets topLevelDefns
    modify $ \st -> st {
        topLevelDefns = M.insert sym loc tlds
        }
    k expr
