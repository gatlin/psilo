---
title: "psilo"
...

This is the main program outline. If an argument is present on the command line
then we execute the program in that file and halt. Otherwise we fire up a repl.

> module Main where
>
> import Parser
> import Syntax
> import Evaluator
>
> import Control.Monad.Trans
> import System.Console.Haskeline
>
> import System.Environment
> import System.IO

`eval` amounts to taking a line of code (a line in the repl, an expression
otherwise), getting the `Expr` value from the parser and then running a
`Machine` with said value.

> eval :: String -> IO ()
> eval line = do
>     let res = parseTopLevel line
>     case res of
>         Left err -> print err
>         Right ex -> mapM_ execute (ex :: [Expr ()])
>
>     where execute v = do
>               (val, _) <- runMachine . interpret $ v
>               putStrLn . show $ val

The repl is nothing more than calling `eval` in an endless loop.

> repl :: IO ()
> repl = runInputT defaultSettings loop
>     where
>     loop = do
>         minput <- getInputLine "psilo> "
>         case minput of
>             Nothing -> outputStrLn "Goodbye."
>             Just input -> (liftIO $ eval input) >> loop

If we execute `eval` on the contents of a file of code, we have a normal
interpreter:

> execFile :: String -> IO ()
> execFile fname = readFile fname >>= eval

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         []      -> repl >> return ()
>         [fname] -> execFile fname >> return ()
