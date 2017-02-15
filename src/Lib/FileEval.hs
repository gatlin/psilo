{-# LANGUAGE OverloadedStrings #-}

module Lib.FileEval where

import Lib.Syntax
import Lib.Parser
import Lib.Compiler.StackVM
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

{-
mapM :: (a -> m b) -> t a -> m (t b)

foldM :: (b -> a -> m b) -> b -> t a -> m b
-}

interpret_file :: Bool -> Bool ->  FilePath -> IO ()
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
