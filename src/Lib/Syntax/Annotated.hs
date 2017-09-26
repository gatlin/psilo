module Lib.Syntax.Annotated where

import Lib.Syntax.Symbol
import Lib.Syntax.Core
import Lib.Syntax.Surface (SurfaceExpr (), SurfaceAst)
import Control.Monad.Free
import Control.Comonad
import Control.Comonad.Cofree

-- | Annotated Expressions
-- An annotated expression is a 'CoreExpr' inverted, with every branch
-- containing an annotation of some carrier type.
type AnnotatedExpr = Cofree CoreAst

-- | Converts a 'CoreExpr' fresh out of the parser into an 'AnnotatedExpr'.
annotated :: Functor f => Free f () -> Cofree f ()
annotated (Free m) = () :< fmap annotated m
