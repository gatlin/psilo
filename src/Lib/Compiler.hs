module Lib.Compiler
    ( Compiler
    , compile
    , compileWithLogs
    , Log
    , logMsg
    , throwError -- re-export, it's a good name
    )
where

import Lib.Errors
import Control.Monad.Except
import Control.Monad.Writer

type Log = [String]

type Compiler = WriterT Log (Except PsiloError)

compile :: Compiler a -> Either PsiloError a
compile = fmap fst . compileWithLogs

compileWithLogs :: Compiler a -> Either PsiloError (a, Log)
compileWithLogs = runExcept . runWriterT

logMsg :: String -> Compiler ()
logMsg = tell . return
