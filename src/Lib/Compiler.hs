module Lib.Compiler
    ( Compiler
    , Log
    , logMsg
    , compile
    , compileWithLogs
    , throwError
    )
where

import Lib.Errors
import Control.Monad.Except
import Control.Monad.Writer

type Log = [String]

type Compiler = WriterT Log (Except PsiloError)

compileWithLogs :: Compiler a -> Either PsiloError (a, Log)
compileWithLogs = runExcept . runWriterT

compile :: Compiler a -> Either PsiloError a
compile = fmap fst . compileWithLogs

logMsg :: String -> Compiler ()
logMsg msg = tell [msg]
