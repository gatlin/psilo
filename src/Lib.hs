module Lib
    ( start
    ) where

import Prelude hiding (map, take, filter, sequence, mapM)
import Control.Monad (forM_)
import Tubes
import Lib.Parser
import Lib.Syntax
import Lib.Runtime
import Lib.Interpreter

tests :: [String]
tests = [ "((\\ (x) (* x x)) 5)"
        , "\"strings work\""
        , "((lambda (name age) (lambda (f) (f name age))) \"gatlin\" 28)"
        ]

start :: IO ()
start = forM_ tests $ \test ->
    runTube $ sample (parse test)
           >< take 1
           >< map fst
           >< map interpret
           >< mapM doRuntime
           >< map show
           >< pour display
