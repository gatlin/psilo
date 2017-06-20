{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.REPL where

import Prelude hiding (map, take)
import qualified Prelude as P
import Lib.Parser
import Lib.FileEval
import Lib.Types
import Lib.Util
import Lib.Syntax
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Free
import Data.Char (isSpace)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Map (Map)
import qualified Data.Map as M

-- self-explanatory
ltrim = dropWhile isSpace

-- | We maintain a list of definitions and a type environment for the session
data ReplState = ReplState
    { defns :: Map Symbol (CoreExpr ())
    , typeEnv :: TypeEnv
    } deriving (Show)

defaultReplState :: ReplState
defaultReplState = ReplState M.empty mempty

-- | The kinds of exceptions that may be raised during REPL execution
data ReplError
    = ParseError Symbol
    | TypeCheckError TypeError
    | UndefinedSymbol Symbol
    | UnknownCommand Symbol
    deriving (Eq, Show)

-- | A monad encapsulating a REPL session
type Repl = StateT ReplState (ExceptT ReplError (InputT IO))

data ReplExpr
    = ReplDefn Definition
    | ReplCore (CoreExpr ())
    deriving (Show, Eq)

runRepl :: ReplState -> Repl a -> IO (Either ReplError a)
runRepl rs m = runInputT defaultSettings $ runExceptT $ evalStateT m rs

handleError :: ReplError -> Repl ()
handleError err = do
    liftIO . putStrLn . show $ err
    repl

-- | Called by the outside
replMain :: IO ()
replMain = do
    runRepl defaultReplState $ repl `catchError` handleError
    return ()

replParse :: String -> Repl ()
replParse src = do
    mParsed <- parse_expr $ Text.pack src
    case mParsed of
        Nothing -> throwError $ ParseError "T_T"
        Just e@(Free (DefS _ _)) -> do
            case surfaceToDefinition e of
                Nothing -> throwError $ ParseError "T_T"
                Just defn@(Define sym expr) -> do
                    replTypeCheckDefn defn
                    ds <- gets defns
                    modify $ \st -> st {
                        defns = M.insert sym expr ds }

        Just e -> liftIO . putStrLn $ src

    -- | Type checks a 'Definition' during a REPL session
replTypeCheckDefn :: Definition -> Repl ()
replTypeCheckDefn defn@(Define sym expr) = do
    te <- gets typeEnv
    case typecheck_defns [defn] te of
        Left err -> do
            throwError $ TypeCheckError err
        Right ((ty:[]), _) -> do
            liftIO . putStrLn $
                sym ++ " : " ++ (show ty)
            modify $ \st -> st {
                typeEnv = extendEnv te (sym, ty) }

-- | Executes REPL commands
replCommand :: String -> Repl ()
replCommand cmd = case break isSpace cmd of
    ("beep", _) -> do
        liftIO . putStrLn $ "boop"
    ("t", sym) -> do
        te <- gets typeEnv
        case envLookup te (ltrim sym) of
            Nothing -> throwError $ UndefinedSymbol sym
            Just ty -> do
                liftIO . putStrLn $
                    sym ++ " : " ++ (show ty)
    (_, _) -> throwError $ UnknownCommand cmd

-- | The actual REPL loop.
repl :: Repl ()
repl = do
    minput <- lift $ lift $ getInputLine "% "
    case fmap ltrim minput  of
        Nothing -> return ()
        Just (':':cmd) -> do
            replCommand cmd
            repl
        Just input -> do
            replParse input
            repl
