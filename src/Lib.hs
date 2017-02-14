module Lib
    ( replMain
--    , interpret
    , interpret_file
    , parse_expr
    , parse_multi
    ) where

import Prelude hiding (map, take, filter, sequence, mapM)
import Control.Monad (forM_)
import Tubes
import Lib.Parser
import Lib.Syntax
import Lib.REPL
import Lib.FileEval
import Lib.Compiler.StackVM
