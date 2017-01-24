{-# LANGUAGE OverloadedStrings #-}

module Lib.REPL where

import Prelude hiding (map, take)
import qualified Prelude as P
import Lib.Interpreter
import Lib.Runtime
import Lib.Parser
import Lib.FileEval
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Tubes

ltrim = dropWhile isSpace

replMain :: IO ()
replMain = runInputT defaultSettings (loop defaultRuntimeState) where
    loop rtState = do
        minput <- getInputLine "% "
        let minput' = fmap ltrim minput
        case minput' of
            Nothing -> return ()
            Just (':':cmd) -> case break isSpace cmd of
                ("store",_) -> do
                    liftIO . putStrLn . show $ rtState
                    loop rtState
                ("load", filePath) -> do
                    rtState' <- liftIO $ do
                        file_contents <- TextIO.readFile $ ltrim filePath
                        defns <- parse_multi file_contents
                        load_defns defns rtState
                    loop rtState'
            Just input -> do
                mParsed <- parse_expr $ Text.pack input
                case mParsed of
                    Nothing -> do
                        liftIO $ putStrLn "Parser error"
                        loop rtState
                    Just parsed -> do
                        (result, rtState') <- liftIO $ runRuntime rtState $
                            interpret parsed
                        liftIO . putStrLn . show $ result
                        loop rtState'
