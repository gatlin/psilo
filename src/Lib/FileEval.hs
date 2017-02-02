{-# LANGUAGE OverloadedStrings #-}

module Lib.FileEval where

import Lib.Syntax
import Lib.Runtime
import Lib.Parser
import Lib.Compiler.StackVM
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

load_defns :: [CoreExpr ()] -> RuntimeState -> IO RuntimeState
load_defns _ x = return x
{-
load_defns [] rtState = return rtState
load_defns (defn:defns) rtState = do
    (_, rtState') <- runRuntime rtState $ interpret defn
    load_defns defns rtState'
-}

interpret_file :: FilePath -> IO ()
interpret_file inFile = do
    file_contents <- TextIO.readFile inFile
    defns <- parse_multi file_contents
    compiled <- mapM codegen defns >>= return . concat
    putStrLn . show $ compiled
    -- make sure to not execute main's return
    st <- run (take ((length compiled) - 1) compiled)
    putStrLn . show $ st
    return ()
