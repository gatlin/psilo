module Lib.Compiler
    ( Compiler
    , Log
    , logMsg
    , compile
    , compileWithLogs
    , throwError
    )
where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Lib.Errors

type Log = [String]

type CompilerState = ()

defaultCompilerState :: CompilerState
defaultCompilerState = ()

type Compiler = StateT CompilerState (WriterT Log (Except PsiloError))

compileWithLogs :: Compiler a -> Either PsiloError (a, Log)
compileWithLogs = runExcept .
                  runWriterT .
                  (flip evalStateT) defaultCompilerState

compile :: Compiler a -> Either PsiloError a
compile = fmap fst . compileWithLogs

logMsg :: String -> Compiler ()
logMsg msg = tell [msg]
