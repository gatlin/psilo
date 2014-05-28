module Main where

import Parser
import Syntax
import Evaluator

import Control.Monad.Trans
import System.Console.Haskeline

import System.Environment
import System.IO

eval :: String -> IO ()
eval line = do
    let res = parseTopLevel line
    case res of
        Left err -> print err
        Right ex -> mapM_ execute (ex :: [Expr ()])

    where execute v = do
              (val, _) <- runMachine . interpret $ v
              putStrLn . show $ val

repl :: IO ()
repl = runInputT defaultSettings loop
    where
    loop = do
        minput <- getInputLine "psilo> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> (liftIO $ eval input) >> loop

execFile :: String -> IO ()
execFile fname = readFile fname >>= eval

main :: IO ()
main = do
    args <- getArgs
    case args of
        []      -> repl >> return ()
        [fname] -> execFile fname >> return ()
