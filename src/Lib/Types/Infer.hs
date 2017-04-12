module Lib.Types.Infer
where

import Lib.Types.Pred
import Lib.Types.Assump
import Lib.Types.TIMonad

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)
