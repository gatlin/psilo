---
title: "psilo"
...

This is the main program outline. If an argument is present on the command line
then we execute the program in that file and halt. Otherwise we fire up a repl.

> {-# LANGUAGE RecordWildCards #-}

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
> import Control.Monad (forM, forM_, when)
> import Data.List (partition)
> import Control.Comonad.Cofree
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
>     , optParsed :: Bool
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
>     <*> switch (
>         long "parsed" <> short 'p'
>               <> help "Show the parser output" )

The repl is nothing more than calling `eval` in an endless loop.

> repl :: CmdLnOpts -> IO ()
> repl os@CmdLnOpts{..} = runInputT defaultSettings (loop newMachineState []) where
>     loop st types = do
>         minput <- getInputLine "psilo> "
>         case minput of
>             Nothing -> outputStrLn "Goodbye."
>             Just input -> if (take 5 input == ":type")
>                     then do
>                         liftIO . putStrLn . show $ lookup (drop 6 input) types
>                         loop st types
>                     else do
>                         let Right ast = parseTopLevel input
>                         let ast'      = (ast !! 0) :: Expr ()
>                         let typed = typeTree $ cofreeMu ast'
>                         case typed of
>                             Nothing -> (liftIO $ putStrLn "Type error") >>
>                                 loop st types
>                             Just ty -> do
>                               when optParsed $ do
>                                   liftIO . putStrLn . show $ ast'
>                               ((ret, log), st') <- liftIO $ runMachine st $ (eval ast')
>                               liftIO . putStrLn . show $ ret
>                               when optConLog $ do
>                                   liftIO . putStrLn $ "Log\n---"
>                                   displayLog log
>                               when optState $ do
>                                   liftIO . putStrLn . show $ st'
>                               loop st' $ types' ast' ty types
>     types' (Free (ADefine sym _)) ty types = (sym, ty) : types
>     types'  _                      _  types = types


> displayLog log = liftIO $ forM_ log putStrLn

> main :: IO ()
> main = execParser opts >>= start

> start :: CmdLnOpts -> IO ()
> start os = case doRepl of
>     True      -> repl os
>     _         -> return ()
>     where
>         doRepl = optRepl os

> opts :: ParserInfo CmdLnOpts
> opts = info (cmdLnOpts <**> helper)
>     ( fullDesc <> progDesc "Run psilo programs" <> header "psilo" )
