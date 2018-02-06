module Lib.Syntax.Annotated where

import Lib.Compiler
import Lib.Errors
import Lib.Syntax.Symbol
import Lib.Syntax.Core
import Lib.Syntax.Surface (SurfaceExpr (), SurfaceAst)
import Control.Monad.Free
import Control.Comonad
import Control.Comonad.Cofree
import Data.Traversable

-- | Annotated Expressions
-- An annotated expression is a 'CoreExpr' inverted, with every branch
-- containing an annotation of some carrier type.
type AnnotatedExpr = Cofree CoreAst

-- | Converts a 'CoreExpr' fresh out of the parser into an 'AnnotatedExpr'.
annotated :: Traversable f => Free f () -> Compiler (Cofree f ())
annotated (Pure _) = throwError $ PreprocessError "honestly idk"
annotated (Free m) = fmap (() :<) $ traverse annotated m
