{-# LANGUAGE OverloadedStrings #-}

module Lib.FileEval where

import Lib.Syntax
import Lib.Parser
import Lib.Preprocessor
import Lib.Compiler.StackVM
import Lib.Errors
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Comonad.Cofree
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Maybe (fromJust)

-- | reads a file, parses it, and returns top level declarations
process_file :: FilePath -> ExceptT PsiloError IO [TopLevel]
process_file file_path = do
    file_contents <- liftIO $ TextIO.readFile file_path
    defns <- parse_multi $ removeComments file_contents
    preprocess $ mapM surfaceToTopLevel defns
