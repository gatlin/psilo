{-# LANGUAGE OverloadedStrings #-}

module Lib.FileEval where

import Lib.Syntax
import Lib.Runtime
import Lib.Interpreter
import Lib.Parser
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Tubes

load_defns :: [CoreExpr ()] -> RuntimeState -> IO RuntimeState
load_defns _ x = return x
{-
load_defns [] rtState = return rtState
load_defns (defn:defns) rtState = do
    (_, rtState') <- runRuntime rtState $ interpret defn
    load_defns defns rtState'
-}

interpret_file :: FilePath -> IO ()
interpret_file _ = return ()
{-
interpret_file inFile = do
    file_contents <- TextIO.readFile inFile
    defns <- parse_multi file_contents
    rtState <- load_defns defns defaultRuntimeState
    mMainExpr <- parse_expr "(main)"
    case mMainExpr of
        Nothing -> error "hoo boy"
        Just main_expr -> do
            (result, _) <- runRuntime rtState $ interpret main_expr
            putStrLn . show $ result
-}
