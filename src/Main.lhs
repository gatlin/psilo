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
>
> import Control.Monad.Trans
> import System.Console.Haskeline
> import Control.Monad.Free
> import Data.Monoid
> import Data.Maybe
> import Control.Monad (forM, forM_)
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
> } deriving Show

> cmdLnOpts :: Parser CmdLnOpts
> cmdLnOpts = CmdLnOpts
>     <$> flag True True ( long "repl" <> short 'r' <> help "Initiate a REPL (default=TRUE)" )
>     <*> switch ( long "console-log" <> short 'l'
>               <> help "Log debug output to the console (default=FALSE)"
>     )

The repl is nothing more than calling `eval` in an endless loop.

> repl :: IO ()
> repl = runInputT defaultSettings loop where
> --    loop store = do
>     loop = do
>         minput <- getInputLine "psilo> "
>         case minput of
>             Nothing -> outputStrLn "Goodbye."
>             Just input -> do
>                 let Right ast = parseTopLevel input
>                 let typed = typeTree $ cofreeMu (ast !! 0)
>                 case typed of
>                     Nothing -> (liftIO $ putStrLn "Type error") >> loop
>                     Just ty -> do
>                       liftIO $ putStrLn $ "Value: " ++ (show ty)
>                       loop

> main :: IO ()
> main = execParser opts >>= start

> start :: CmdLnOpts -> IO ()
> start os = if doRepl then repl else return () where
>     doRepl = optRepl os
>     conLog = optConLog os

> opts :: ParserInfo CmdLnOpts
> opts = info (cmdLnOpts <**> helper)
>     ( fullDesc <> progDesc "Run psilo programs" <> header "psilo" )
