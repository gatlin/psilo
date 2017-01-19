module Main where

import Options.Applicative
import Options.Applicative.Common
import Lib

data CmdLnOpts = CmdLnOpts
    { inputFile :: Maybe String
    , execRepl  :: Bool
    } deriving (Show)

optParser :: Parser CmdLnOpts
optParser = CmdLnOpts
    <$> (optional $ strOption $
         (long "in")
      <> (metavar "FILE_INPUT")
      <> (help "Path to input file."))
    <*> (switch $
         (long "repl")
      <> (short 'r')
      <> (help "Enter a psilo repl"))

main :: IO ()
main = execParser opts >>= begin where
    opts = info (helper <*> optParser)
        ( fullDesc
       <> progDesc "Execute psilo programs."
       <> header "psilo - a practical, safe, interpreted, linear operation language")

test_program = "(def square (\\ (x) (* x x))) (def plus1 (\\ (x) (+ 1 x)))"

begin :: CmdLnOpts -> IO ()
begin cmdLnOpts = case inputFile cmdLnOpts of
    Nothing -> replMain
    Just inFile -> interpret_file inFile
