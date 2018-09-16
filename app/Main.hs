{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Comonad.Cofree
import           Control.Monad              (foldM, forM_, mapM, mapM_, when)
import           Control.Monad.Except
import           Data.Foldable
import           Data.Map                   (Map)
import qualified Data.Map                   as M
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
    } deriving (Show)

optParser :: Parser CmdLnOpts
optParser = CmdLnOpts
    <$> (optional $ strOption $
         (long "in")
      <> (metavar "FILE_INPUT")
      <> (help "Path to input file."))
    {-
    <*> (switch $
         (long "asm")
      <> (short 'm')
      <> (help "Dump generated assembly code (when applicable)"))
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
        contents <- TextIO.readFile inFile
        let result = compileWithLogs $ do
                (topLevel, exprs) <- (process_file contents)
                ce <- transformCE (classes topLevel) mempty
                return (topLevel, ce, exprs)
        case result of
            Left err -> putStrLn . ushow $ err
            Right ((topLevel, ce, exprs), logs) -> do
                forM_ exprs $ putStrLn . show
                forM_ logs $ putStrLn . show
                putStrLn "-----"
                putStrLn . show $ ce
                putStrLn "-----"
                putStrLn . show $ (methods topLevel)
                putStrLn "-----"
                putStrLn . show $ (signatures topLevel)
                putStrLn "-----"
                putStrLn . show $ (typedefs topLevel)
                putStrLn "-----"
                putStrLn . show $ (definitions topLevel)y
--                putStrLn . show $ (definitions topLevel)
--                putStrLn "-----"

process_file :: Text -> Compiler (TopLevel, [SurfaceExpr ()])
process_file file_contents = do
    exprs <- parse_multi $ removeComments file_contents
    topLevels <- preprocess $ do
        toplevel <- foldM surfaceToTopLevel mempty exprs
        boundVarCheck toplevel
        return toplevel
    topLevels' <- typecheck topLevels
    -- now typecheck the methods
    return (topLevels', exprs)
