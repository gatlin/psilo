module Lib.REPL where

import Prelude hiding (map, take)
import qualified Prelude as P
import Lib.Interpreter
import Lib.Runtime
import Lib.Parser
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.IO.Class
import Tubes

replMain :: IO ()
replMain = runInputT defaultSettings (loop defaultRuntimeState) where
    loop rtState = do
        minput <- getInputLine "% "
        case minput of
            Nothing -> return ()
            Just ":store" -> do
                liftIO . putStrLn . show $ storage rtState
                loop rtState
            Just input -> do
                mParsed <- liftIO $ parse input
                case mParsed of
                    Nothing -> liftIO $ putStrLn "Parser error"
                    Just parsed -> do
                        (result, rtState') <- liftIO $ runRuntime rtState $
                            interpret parsed
                        liftIO . putStrLn . show $ result
                        loop rtState'
