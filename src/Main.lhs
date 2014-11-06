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
> import Control.Monad.Free
> import Data.Monoid
> import Data.Maybe
> import Control.Monad (forM)
> import Data.List (partition)
>
> import System.Environment
> import System.IO
> import Text.Parsec

`eval` amounts to taking a list of parsed expressions and evaluating them in
the context of a machine. The result is the state of the machine after it has been run.

> evaluate :: Either ParseError [Expr ()] -> MStore -> IO [((Value,[String]), MStore)]
> evaluate res store = do
>     case res of
>         Left err -> print err >> return [((VNil,[]), store)]
>         Right ex -> mapM execute (ex :: [Expr ()]) >>= return
>
>     where execute v = do
>               ev <- return $ mGlobalEnv store
>               res <- (runMachineWithState store (MEnv ev)) . eval $ v
>               return res

The repl is nothing more than calling `eval` in an endless loop.

> repl :: IO ()
> repl = runInputT defaultSettings (loop initialStore) where
>     loop store = do
>         minput <- getInputLine "psilo> "
>         case minput of
>             Nothing -> outputStrLn "Goodbye."
>             Just input -> do
>                 case input of
>                     ":state" -> liftIO (putStrLn . show $ store) >> loop store
>                     _        -> do
>                         (val, store'):_ <- liftIO $ evaluate (parseTopLevel input) store
>                         liftIO $ putStrLn . show $ val
>                         loop store'

> execFile :: String -> IO ()
> execFile fname = do
>     parsed <- parseFile fname
>     case parsed of
>         Left err -> print err >> return ()
>         Right xs -> do
>             (defns, exprs) <- return $ partition isDefn xs
>             initState <- loop defns initialStore
>             final <- evaluate (Right exprs) initState
>             return ()
>     where isDefn (Free (ADefine _ _)) = True
>           isDefn _                    = False
>           loop [] sto = return sto
>           loop (d:ds) sto = do
>               (_, sto') <- runMachine . eval $ d
>               sto'' <- loop ds sto'
>               return $ sto' <> sto''

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         []      -> repl >> return ()
>         (fname:_) -> execFile fname >> return ()
