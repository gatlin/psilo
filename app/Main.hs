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
                exprs <- forM defns $ \(symbol, expr) -> do
                    let mScheme = envLookup typeEnv symbol
                    when (isJust mScheme) $ do
                        let scheme = fromJust mScheme
                        putStrLn $ symbol ++ " : " ++ (ushow scheme)
                    let lExprs = liftExpr symbol expr
                    putStrLn . show $ convertClosure globals expr
                    return lExprs
                --codegen (emptyModule "hey") $ concat exprs
                return ()

globals = S.toList builtin_syms

process_file :: Text -> Compiler [TopLevel]
process_file file_contents = do
    defns <- parse_multi $ removeComments file_contents
    preprocess $ do
        toplevels <- mapM surfaceToTopLevel defns
        boundVarCheck toplevels
        return toplevels
