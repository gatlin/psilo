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

buildTypeEnv :: [(Symbol, Scheme)] -> TypeEnv
buildTypeEnv = foldl go emptyTypeEnv
    where go tyEnv sig = extendEnv tyEnv sig

begin :: CmdLnOpts -> IO ()
begin cmdLnOpts = case inputFile cmdLnOpts of
    Nothing -> return ()
    Just inFile -> do
        contents <- openFile inFile
        let result = runExcept $ do
                toplevels <- process_file contents
                let (defns, sigs) = splitUp toplevels
                let tyEnv = buildTypeEnv sigs
                (tys, cs) <- typecheck_defns defns tyEnv
                return (toplevels, tys, cs)
        case result of
            Left err -> putStrLn . show $ err
            Right (tls, tys, cs) -> do
                forM_ tys $ putStrLn . show
                putStrLn "---"
                forM_ cs $ putStrLn . show
                putStrLn "---"
                forM_ tls $ putStrLn . show


openFile :: FilePath -> IO Text
openFile = TextIO.readFile

loadTest = do
    contents <- openFile "test.sl"
    let result = runExcept $ do
            toplevels <- process_file contents
            let (defns, sigs) = splitUp toplevels
            let tyEnv = buildTypeEnv sigs
            let defns' = fmap (\(x, y) -> (x, annotated y)) defns
            return $ (toplevels, defns, defns', tyEnv)
    return result

process_file :: Text -> Except PsiloError [TopLevel]
process_file file_contents = do
    defns <- parse_multi $ removeComments file_contents
    preprocess $ do
        toplevels <- mapM surfaceToTopLevel defns
        boundVarCheck toplevels
