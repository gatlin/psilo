{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.REPL where

import Prelude hiding (map, take)
import qualified Prelude as P
import Lib.Parser
import Lib.FileEval
import Lib.Types
import Lib.Types.Scheme
import Lib.Util
import Lib.Syntax ( surfaceToTopLevel
                  , surfaceToCore
                  , CoreAst(..)
                  , CoreExpr
                  , SurfaceAst(..)
                  , SurfaceExpr
                  , TopLevel(..)
                  , Symbol
                  )
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Free
import Data.Char (isSpace)
import Control.Monad (forM_, foldM, guard)
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
    , exprCount :: Int
    } deriving (Show)

defaultReplState :: ReplState
defaultReplState = ReplState M.empty mempty 0

-- | The kinds of exceptions that may be raised during REPL execution
data ReplError
    = ParseError Symbol
    | TypeCheckError TypeError
    | TypeMismatch Symbol
    | UndefinedSymbol Symbol
    | UnknownCommand Symbol
    | OtherError String
    deriving (Eq, Show)

-- | A monad encapsulating a REPL session
type Repl = StateT ReplState (ExceptT ReplError (InputT IO))

runRepl :: ReplState -> Repl a -> IO (Either ReplError a)
runRepl rs m = runInputT defaultSettings $ runExceptT $ evalStateT m rs

-- | For now we just catch exceptions and print them out
handleError :: ReplError -> Repl ()
handleError err = do
    liftIO . putStrLn . show $ err
    replLoop

gensym :: Repl Symbol
gensym = do
    c <- gets exprCount
    modify $ \st -> st { exprCount = c + 1 }
    return $ "sym#" ++ (show c)

-- | Called by the outside
replMain :: IO ()
replMain = do
    runRepl defaultReplState $ replLoop `catchError` handleError
    return ()

replExtendTypeEnv :: Symbol -> Scheme -> Repl ()
replExtendTypeEnv sym scheme = do
    te <- gets typeEnv
    modify $ \st -> st {
        typeEnv = extendEnv te (sym, scheme) }

replLookupTypeEnv :: Symbol -> Repl (Maybe Scheme)
replLookupTypeEnv sym = do
    te <- gets typeEnv
    return $ envLookup te sym

-- | Parse an input string into either a definition or a bare expression
replTypeCheck :: String -> Repl ()
replTypeCheck src = do
    te <- gets typeEnv
    expr <- replParseExpr src
    sym <- gensym
    case (runExcept $ typecheck_defn (sym, expr) te) of
        Left err -> throwError $ TypeCheckError err
        Right (ty, _) -> do
            liftIO . putStrLn . show $ ty

replParseExpr :: String -> Repl (CoreExpr ())
replParseExpr src = do
    mParsed <- parse_expr $ Text.pack src
    case mParsed of
        Left err -> throwError $ ParseError err
        Right surface -> case surfaceToCore surface of
            Nothing -> throwError $ OtherError "Malformed expression"
            Just expr -> return expr

replTopLevel :: [TopLevel] -> Repl ()
replTopLevel topLevels = do
    te <- gets typeEnv
    defns <- foldM
        (\ds topLevel -> do
                case topLevel of
                    (Signature sym scheme) -> do
                        replExtendTypeEnv sym scheme
                        return ds
                    d@(Define sym expr) -> return ((sym, expr):ds))
        []
        topLevels
    let syms = fmap fst defns
    case (runExcept $ typecheck_defns defns te) of
        Left err -> throwError $ TypeCheckError err
        Right (schemes, _) -> forM_ (zip syms schemes) $ \(sym, scm) -> do
            mScm <- replLookupTypeEnv sym
            case mScm of
                Nothing -> replExtendTypeEnv sym scm
                Just old_scm -> if old_scm /= scm
                    then throwError $
                         OtherError $ "Conflicting types for " ++ sym
                    else return ()

-- | Executes REPL shell commands
replCommand :: String -> Repl ()
replCommand cmd = case break isSpace cmd of
    ("beep", _) -> do
        liftIO . putStrLn $ "boop"
    ("t", expr) -> replTypeCheck (ltrim expr)
    ("l", file_path) -> do
        result <- liftIO $ runExceptT $ process_file (ltrim file_path)
        case result of
            Left err -> throwError $ ParseError err
            Right topLevels -> replTopLevel topLevels
    (_, _) -> throwError $ UnknownCommand cmd

-- | The actual REPL loop.
replLoop :: Repl ()
replLoop = do
    minput <- lift $ lift $ getInputLine "% "
    case fmap ltrim minput  of
        Nothing -> return ()
        Just (':':cmd) -> do
            replCommand cmd
            replLoop
        Just input -> do
            replLoop
