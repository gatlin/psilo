{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Options.Applicative.Common
import Data.Monoid ((<>))
import Data.Maybe (isJust, fromJust)
import Control.Monad.Except
import Control.Monad (forM_, mapM, foldM, when)
import Control.Comonad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sort)
import Text.Show.Unicode
import Lib

data CmdLnOpts = CmdLnOpts
    { inputFile :: Maybe String
--    , debugOut :: Bool
    , asmDump :: Bool
--    , execRepl  :: Bool
    } deriving (Show)

optParser :: Parser CmdLnOpts
optParser = CmdLnOpts
    <$> (optional $ strOption $
         (long "in")
      <> (metavar "FILE_INPUT")
      <> (help "Path to input file."))
    <*> (switch $
         (long "asm")
      <> (short 'm')
      <> (help "Dump generated assembly code (when applicable)"))

main :: IO ()
main = execParser opts >>= begin where
    opts = info (helper <*> optParser)
        ( fullDesc
       <> progDesc "Execute psilo programs."
       <> header "psilo"
        )

begin :: CmdLnOpts -> IO ()
begin cmdLnOpts = case inputFile cmdLnOpts of
    Nothing -> return ()
    Just inFile -> do
        contents <- TextIO.readFile inFile
        let result = compileWithLogs $ do
                toplevels <- process_file contents
                let (defns, sigs) = splitUp toplevels
                let tyEnv = buildTypeEnv sigs
                typeEnv <- typecheck defns tyEnv
                return (typeEnv, defns)
        case result of
            Left err -> putStrLn . ushow $ err
            Right ((typeEnv, defns), logs) -> do
                putStrLn "Logs\n-----"
                forM_ logs putStrLn
                putStrLn "-----"
                forM_ defns $ \(symbol, _) -> do
                    let mScheme = envLookup typeEnv symbol
                    when (isJust mScheme) $ do
                        let scheme = fromJust mScheme
                        putStrLn $ symbol ++ " : " ++ (ushow scheme)

process_file :: Text -> Compiler [TopLevel]
process_file file_contents = do
    defns <- parse_multi $ removeComments file_contents
    preprocess $ do
        toplevels <- mapM surfaceToTopLevel defns
        boundVarCheck toplevels
        return toplevels
