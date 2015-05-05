{-# LANGUAGE RecordWildCards #-}

module Main where

import Parser (parseFile, parseTopLevel)
import Syntax
import Evaluator

import Control.Monad.Trans
import System.Console.Haskeline
import Control.Monad.Free
import Data.Monoid
import Data.Maybe
import Control.Monad (forM, forM_, when )
import Data.List (partition)
import Control.Comonad.Cofree

import System.Environment
import System.IO
import Text.Parsec

import Options.Applicative

import Test

data CmdLnOpts = CmdLnOpts {
      optRepl   :: Bool
    , optConLog :: Bool
    , optState  :: Bool
    , optParsed :: Bool
    , optFile   :: String
} deriving Show

cmdLnOpts :: Parser CmdLnOpts
cmdLnOpts = CmdLnOpts
    <$> flag False True (
        long "repl" <> short 'r' <> help "Initiate a REPL (default=TRUE)" )
    <*> switch (
        long "console-log" <> short 'l'
              <> help "Log debug output to the console (default=FALSE)" )
    <*> switch (
        long "show-state" <> short 's'
              <> help "Display the machine state after each execution" )
    <*> switch (
        long "parsed" <> short 'p'
              <> help "Show the parser output" )
    <*> strOption (
        long "input" <>
        short 'i' <>
        metavar "FILENAME" <>
        help "Execute a file" <> value "")
{-

repl :: CmdLnOpts -> MachineState -> IO ()
repl os@CmdLnOpts{..} st = runInputT defaultSettings (loop st) where
    loop st = do
        minput <- getInputLine "psilo> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
                let wtf = parseTopLevel input
                case wtf of
                    Left fuckyou -> do
                        liftIO . putStrLn $ "wtf: " ++ (show fuckyou)
                    Right ast -> do
                        let ast'      = (ast !! 0) :: Expr ()
                        let maybeType = typeTree $ cofreeMu $ toUnary ast'
                        case maybeType of
                            Nothing -> liftIO . putStrLn $ "Bad type"
                            Just ty -> do
                                -- TODO: reuse the type constraints
                                when optParsed $ do
                                    liftIO . putStrLn . show $ ast'
                                ((ret, log), st') <- liftIO $
                                    runMachine st $ (eval ast') >>= strict
                                liftIO . putStrLn . show $ ret
                                when optConLog $ do
                                    liftIO . putStrLn $ "Log\n---"
                                    displayLog log
                                when optState $ do
                                    liftIO . putStrLn . show $ st'
                                loop st'

execFile :: CmdLnOpts -> IO ()
execFile os@CmdLnOpts{..} = do
    parsed <- parseFile optFile
    case parsed of
        Left err -> print err >> return ()
        Right xs -> do
            (defns, exprs) <- return $ partition isDefn xs
            defnsTypes <- forM defns $ \defn -> return $ typeTree $ cofreeMu $
                toUnary defn
            -- todo: do something with type information
            st <- insertDefns defns newMachineState
            case length exprs of
                0 -> repl os st
                _ -> do
                    let expr = exprs !! 0
                    ((ret, log), st') <- runMachine st $ eval expr
                    when optState $ do
                            putStrLn . show $ st'
   where
       isDefn (Free (ADefine _ _)) = True
       isDefn _                    = False
       insertDefns [] st = return st
       insertDefns ds st = do
           (_, st') <- runMachine st $ forM_ ds eval
           return st'

displayLog log = liftIO $ forM_ log putStrLn

main :: IO ()
main = execParser opts >>= start

start :: CmdLnOpts -> IO ()
start os@CmdLnOpts{..} = case optRepl of
    True      -> repl os newMachineState
    _         -> case optFile of
        "" -> return ()
        fileName -> execFile os

opts :: ParserInfo CmdLnOpts
opts = info (cmdLnOpts <**> helper)
    ( fullDesc <> progDesc "Run psilo programs" <> header "psilo" )
    -}

main = return ()

