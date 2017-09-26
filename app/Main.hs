{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Options.Applicative.Common
import Data.Monoid ((<>))
import Control.Monad.Except
import Control.Monad (forM_, mapM, foldM)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
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

type Defn = (Symbol, AnnotatedExpr ())
type Sig = (Symbol, Scheme)

splitUp
    :: [TopLevel]
    -> ([Defn], [Sig])
splitUp = foldl go ([], [])
    where go (defns, sigs) tl =
              case tl of
                  (Define s d) -> ((s, d):defns, sigs)
                  (Signature s t) -> (defns, (s, t):sigs)

buildTypeEnv :: [Sig] -> TypeEnv
buildTypeEnv = foldl go emptyTypeEnv
    where go tyEnv sig = extendEnv tyEnv sig

begin :: CmdLnOpts -> IO ()
begin cmdLnOpts = case inputFile cmdLnOpts of
    Nothing -> return ()
    Just inFile -> do
        file_contents <- TextIO.readFile inFile
        result <- return . runExcept $ do
            toplevels <- process_file file_contents
            let (defns, sigs) = splitUp toplevels
            let tyEnv = buildTypeEnv sigs
            (sigs, constraints) <- typecheck_defns defns tyEnv
            return (toplevels, sigs, constraints)
        case result of
            Left err -> putStrLn . show $ err
            Right (toplevels, schemes, constraints) -> do
                putStrLn "Type schemes"
                putStrLn "---"
                forM_ schemes $ \ (sym, sig) ->
                    putStrLn $ sym ++ " : " ++ (show sig)
                putStrLn "---"
                putStrLn "Top Level Definitions"
                putStrLn "---"
                forM_ toplevels $ putStrLn . show
                putStrLn "---"
                putStrLn "Constraints"
                putStrLn "---"
                forM_ constraints $ putStrLn . show


process_file :: Text -> Except PsiloError [TopLevel]
process_file file_contents = do
    defns <- parse_multi $ removeComments file_contents
    preprocess $ mapM surfaceToTopLevel defns
