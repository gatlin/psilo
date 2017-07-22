{-# LANGUAGE OverloadedStrings #-}

module Lib.FileEval where

import Lib.Syntax
import Lib.Parser
import Lib.Compiler.StackVM
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Maybe (fromJust)

process_file :: FilePath -> ExceptT String IO [Definition]
process_file file_path = do
    file_contents <- liftIO $ TextIO.readFile file_path
    defns <- liftIO $ parse_multi $ removeComments file_contents
    case defns of
        Left err -> throwError err
        Right defns' -> return $ fmap (fromJust . surfaceToDefinition) defns'
{-
interpret_file optDebug optAsm inFile = do
    file_contents <- TextIO.readFile inFile
    defns <- parse_multi $ removeComments file_contents
    compiled' <- runCodegenT newCodegenContext newCodegenState $ do
        compiled <- foldM (\ds c -> codegen c >>= \c' -> return $ c' : ds) [] defns
        return $ concat . reverse $ compiled
    when optAsm $ forM_ compiled' $ putStrLn . show
    st <- run (take ((length compiled') - 1) compiled')
    when (optDebug && (not optAsm)) $
        putStrLn . show $ stackPeek $ machineStack st
    return ()
-}
