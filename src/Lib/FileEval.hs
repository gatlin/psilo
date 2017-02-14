{-# LANGUAGE OverloadedStrings #-}

module Lib.FileEval where

import Lib.Syntax
import Lib.Parser
import Lib.Compiler.StackVM
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

interpret_file :: Bool -> Bool ->  FilePath -> IO ()
interpret_file optDebug optAsm inFile = do
    file_contents <- TextIO.readFile inFile
    defns <- parse_multi $ removeComments file_contents
    compiled <- mapM codegen defns >>= return . concat
    -- make sure to not execute main's return
    st <- run (take ((length compiled) - 1) compiled)
    when optAsm $ forM_ compiled $ putStrLn . show
    when (optDebug && (not optAsm)) $
        putStrLn . show $ stackPeek $ machineStack st
    return ()
