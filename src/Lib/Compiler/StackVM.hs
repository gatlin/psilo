 module Lib.Compiler.StackVM
    ( run
    , codegen
    , stackPeek
    , newCodegenContext
    , newCodegenState
    , runCodegenT
    , MachineT(..)
    , MachineState(..)
    )
where

import Lib.Compiler.StackVM.Machine
import Lib.Compiler.StackVM.Codegen
