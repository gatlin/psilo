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
                        file_contents <- readFile $ ltrim filePath
                        defns <- parse_multi $ Source $ each file_contents
                        load_defns (reverse defns) rtState
                    loop rtState'
            Just input -> do
                mParsed <- liftIO $ parse $ Source (each input)
                case mParsed of
                    Nothing -> (liftIO $ putStrLn "Parser error") >> loop rtState
                    Just (parsed, _) -> do
                        (result, rtState') <- liftIO $ runRuntime rtState $
                            interpret parsed
                        liftIO . putStrLn . show $ result
                        loop rtState'
