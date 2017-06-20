module Main where

import Options.Applicative
import Options.Applicative.Common
import Data.Monoid ((<>))
import Lib

data CmdLnOpts = CmdLnOpts
    { inputFile :: Maybe String
    , debugOut :: Bool
    , asmDump :: Bool
    , execRepl  :: Bool
    } deriving (Show)

optParser :: Parser CmdLnOpts
optParser = CmdLnOpts
    <$> (optional $ strOption $
         (long "in")
      <> (metavar "FILE_INPUT")
      <> (help "Path to input file."))
    <*> (switch $
         (long "debug")
      <> (short 'd')
      <> (help "Enable debug output"))
    <*> (switch $
         (long "asm")
      <> (short 'm')
      <> (help "Dump generated assembly code (when applicable)"))
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

begin :: CmdLnOpts -> IO ()
begin cmdLnOpts = case inputFile cmdLnOpts of
    Nothing -> replMain
    Just inFile -> interpret_file (debugOut cmdLnOpts)
                   (asmDump cmdLnOpts) inFile
