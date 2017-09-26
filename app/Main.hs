{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Options.Applicative.Common
import Data.Monoid ((<>))
import Control.Monad.Except
import Control.Monad (forM_, mapM, foldM)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Lib ( parse_multi
           , removeComments
           , preprocess
           , surfaceToTopLevel
           , PsiloError
           , TopLevel(..)
           )

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
{-
    <*> (switch $
         (long "debug")
      <> (short 'd')
      <> (help "Enable debug output"))
-}
    <*> (switch $
         (long "asm")
      <> (short 'm')
      <> (help "Dump generated assembly code (when applicable)"))
{-
    <*> (switch $
         (long "repl")
      <> (short 'r')
      <> (help "Enter a psilo repl"))
-}

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
        file_contents <- TextIO.readFile inFile
        result <- return $ process_file file_contents
        case runExcept result of
            Left err -> putStrLn . show $ err
            Right defns -> forM_ defns $ putStrLn . show

process_file :: Text -> Except PsiloError [TopLevel]
process_file file_contents = do
    defns <- parse_multi $ removeComments file_contents
    preprocess $ mapM surfaceToTopLevel defns
