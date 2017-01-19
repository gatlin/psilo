module Lib.FileEval where

import Lib.Syntax
import Lib.Runtime
import Lib.Interpreter
import Lib.Parser
import Control.Monad
import Tubes

load_defns :: [CoreExpr ()] -> RuntimeState -> IO RuntimeState
load_defns [] rtState = return rtState
load_defns (defn:defns) rtState = do
    (_, rtState') <- runRuntime rtState $ interpret defn
    load_defns defns rtState'

interpret_file :: FilePath -> IO ()
interpret_file inFile = do
    file_contents <- readFile inFile
    defns <- parse_multi $ Source $ each file_contents
    rtState <- load_defns defns defaultRuntimeState
    mMainExpr <- parse $ Source $ each "(main)"
    case mMainExpr of
        Nothing -> error "hoo boy"
        Just (main_expr, _) -> do
            (result, _) <- runRuntime rtState $ interpret main_expr
            putStrLn . show $ result
