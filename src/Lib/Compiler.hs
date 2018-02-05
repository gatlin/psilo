module Lib.Compiler
where

import Lib.PsiloError
import Control.Monad.Except

type Compiler = Except PsiloError a

compile :: Compiler a -> Either PsiloError a
compile = runExcept
