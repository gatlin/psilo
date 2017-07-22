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

process_file :: FilePath -> ExceptT String IO [TopLevel]
process_file file_path = do
    file_contents <- liftIO $ TextIO.readFile file_path
    defns <- liftIO $ parse_multi $ removeComments file_contents
    case defns of
        Left err -> throwError err
        Right defns' -> return $ fmap (fromJust . surfaceToTopLevel) defns'
