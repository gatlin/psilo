---
title: "psilo"
...

This is the main program outline. If an argument is present on the command line
then we execute the program in that file and halt. Otherwise we fire up a repl.

> module Main where
>
> import Parser (parseFile, parseTopLevel)
> import Syntax
> import Typechecker
> import Evaluator
>
> import Control.Monad.Trans
> import System.Console.Haskeline
> import Control.Monad.Free
> import Data.Monoid
> import Data.Maybe
> import Control.Monad (forM, forM_, unless)
> import Data.List (partition)
>
> import System.Environment
> import System.IO
> import Text.Parsec
>
> import Options.Applicative

> data CmdLnOpts = CmdLnOpts {
>       optRepl   :: Bool
>     , optConLog :: Bool
>     , optState  :: Bool
> } deriving Show

> cmdLnOpts :: Parser CmdLnOpts
> cmdLnOpts = CmdLnOpts
>     <$> flag True True (
>         long "repl" <> short 'r' <> help "Initiate a REPL (default=TRUE)" )
>     <*> switch (
>         long "console-log" <> short 'l'
>               <> help "Log debug output to the console (default=FALSE)" )
>     <*> switch (
>         long "show-state" <> short 's'
>               <> help "Display the machine state after each execution" )

The repl is nothing more than calling `eval` in an endless loop.

> repl :: Bool -> Bool -> IO ()
> repl doLog showSt = runInputT defaultSettings (loop newMachineState) where
>     loop st = do
>         minput <- getInputLine "psilo> "
>         case minput of
>             Nothing -> outputStrLn "Goodbye."
>             Just input -> do
>                 let Right ast = parseTopLevel input
>                 let ast'      = (ast !! 0) :: Expr ()
>                 let typed = typeTree $ cofreeMu ast'
>                 case typed of
>                     Nothing -> (liftIO $ putStrLn "Type error") >> loop st
>                     Just ty -> do
>                       liftIO $ putStrLn $ "Type: " ++ (show ty)
>                       ((ret, log), st') <- liftIO $ runMachine st $ (eval ast')
>                       liftIO . putStrLn $ "Result: " ++ (show ret)
>                       unless (doLog == False) $ do
>                           liftIO . putStrLn $ "Log\n---"
>                           displayLog log
>                       unless (showSt == False) $
>                           liftIO . putStrLn . show $ st'
>                       loop st'

> displayLog log = liftIO $ forM_ log putStrLn

> main :: IO ()
> main = execParser opts >>= start

> start :: CmdLnOpts -> IO ()
> start os = if doRepl then repl conLog showSt else return () where
>     doRepl = optRepl os
>     conLog = optConLog os
>     showSt = optState os

> opts :: ParserInfo CmdLnOpts
> opts = info (cmdLnOpts <**> helper)
>     ( fullDesc <> progDesc "Run psilo programs" <> header "psilo" )
