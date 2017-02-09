 module Lib.Compiler.StackVM
    ( run
    , codegen
    , stackPeek
    , MachineT(..)
    , MachineState(..)
    )
where

import Lib.Compiler.StackVM.Machine
import Lib.Compiler.StackVM.Codegen
