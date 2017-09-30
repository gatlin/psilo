{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Options.Applicative.Common
import Data.Monoid ((<>))
import Control.Monad.Except
import Control.Monad (forM_, mapM, foldM)
import Control.Comonad
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Map (Map)
import qualified Data.Map as M
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
        let result = runExcept $ do
                toplevels <- process_file contents
                let (defns, sigs) = splitUp toplevels
                let tyEnv = buildTypeEnv sigs
                typecheck defns tyEnv
        case result of
            Left err -> putStrLn . show $ err
            Right (tys, TypeEnv te) -> do
--                forM_ (M.toList te) $ putStrLn . show
                forM_ tys $ \(sym, expr) -> do
                    putStrLn $ sym ++ " : " ++ (show $ extract expr)

process_file :: Text -> Except PsiloError [TopLevel]
process_file file_contents = do
    defns <- parse_multi $ removeComments file_contents
    preprocess $ do
        toplevels <- mapM surfaceToTopLevel defns
        boundVarCheck toplevels
        return toplevels
