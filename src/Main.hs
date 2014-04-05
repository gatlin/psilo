module Main where

import Parser
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline

import System.Environment
import System.IO

process :: String -> IO ()
process line = do
    let res = parseTopLevel line
    case res of
        Left err -> print err
        Right ex -> mapM_ print (ex :: [PExpr ()])

repl :: IO ()
repl = runInputT defaultSettings loop
    where
    loop = do
        minput <- getInputLine "psilo> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> (liftIO $ process input) >> loop

execFile :: String -> IO ()
execFile fname = readFile fname >>= process

main :: IO ()
main = do
    args <- getArgs
    case args of
        []      -> repl >> return ()
        [fname] -> execFile fname >> return ()
