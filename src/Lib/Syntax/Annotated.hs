module Lib.Syntax.Annotated where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Monad.Free
import           Data.Traversable
import           Lib.Compiler
import           Lib.Errors
import           Lib.Syntax.Core
import           Lib.Syntax.Surface     (SurfaceAst, SurfaceExpr ())
import           Lib.Syntax.Symbol

-- | Annotated Expressions
-- An annotated expression is a 'CoreExpr' inverted, with every branch
-- containing an annotation of some carrier type.
type AnnotatedExpr = Cofree CoreAst

-- | Converts a 'CoreExpr' fresh out of the parser into an 'AnnotatedExpr'.
annotated :: Traversable f => Free f () -> Compiler (Cofree f ())
annotated (Pure _) = throwError $ PreprocessError "Error annotating syntax tree"
annotated (Free m) = fmap (() :<) $ traverse annotated m
