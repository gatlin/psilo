{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Comonad.Cofree
import           Control.Monad              (foldM, forM_, mapM, mapM_, when)
import           Control.Monad.Except
import           Data.Maybe                 (fromJust, isJust)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TextIO
import           Lib
import           Options.Applicative
import           Options.Applicative.Common
import           Text.Show.Unicode

data CmdLnOpts = CmdLnOpts
    { inputFile :: Maybe String
--    , debugOut :: Bool
    , asmDump   :: Bool
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
        let result = compileWithLogs $
                     process_file contents

        case result of
            Left err -> putStrLn . ushow $ err
            Right ((typeEnv, defns), logs) -> do
                putStrLn "Logs\n-----"
                forM_ logs putStrLn
                putStrLn "-----"

process_file :: Text -> Compiler (TypeEnv, [(Symbol, AnnotatedExpr ())])
process_file file_contents = do
    defns <- parse_multi $ removeComments file_contents
    logMsg $ show defns
    toplevels <- preprocess $ do
        toplevels <- mapM surfaceToTopLevel defns >>= return . concat
        boundVarCheck toplevels
        return toplevels
    let (defns, sigs, tds) = splitUp toplevels
    let tyEnv = buildTypeEnv sigs
    logMsg $ ushow tyEnv
    typeEnv <- typecheck defns tyEnv
    return (typeEnv, defns)
