module Lib.Syntax
    ( Symbol
    , builtin_syms
    , CoreExpr
    , CoreAst(..)
    , TopLevel(..)
    , SurfaceExpr
    , SurfaceAst(..)
    , surfaceToCore
    , surfaceToTopLevel
    , aInt
    , aFloat
    , aBool
    , aId
    , aApp
    , aFun
    , aIf
    , aDef
    , AnnotatedExpr
    , annotated
    )
where

import Lib.Syntax.Symbol
import Lib.Syntax.Core
import Lib.Syntax.Surface
import Lib.Syntax.Annotated
import Lib.Syntax.TopLevel

import Control.Monad.Free

-- | Converts a 'SurfaceExpr' to a 'CoreExpr' or fails.
-- TODO needs to make each symbol unique
surfaceToCore :: SurfaceExpr () -> Maybe (CoreExpr ())
surfaceToCore (Free (DefS _ _)) = Nothing
surfaceToCore other = Just $ convert other
    where
        convert (Free (FunS args body)) = Free (FunC args (convert body))
        convert (Free (AppS op erands)) =
            Free (AppC (convert op) (map convert erands))

        convert (Free (IntS n)) = Free (IntC n)
        convert (Free (FloatS n)) = Free (FloatC n)
        convert (Free (BoolS b)) = Free (BoolC b)
        convert (Free (IdS s)) = Free (IdC s)
        convert (Free (IfS c t e)) = Free $
            IfC (convert c) (convert t) (convert e)

-- | Converts a 'SurfaceExpr' to a 'TopLevel' or fails
surfaceToTopLevel :: SurfaceExpr () -> Maybe TopLevel
surfaceToTopLevel (Free (DefS sym expr)) = do
    core <- surfaceToCore expr
    return $ Define sym core

-- for signatures, we want to convert them into 'Signature' values. This
-- involves converting each unique string type variable into legit actual type
-- variables, etc.
{-
        tailRec sym (Free (IfS c t e)) = Free $
            IfC (convert c) (tailRec sym t) (tailRec sym e)

        tailRec sym expr@(Free (AppS (Free (IdS fun)) ops))
            | sym == fun = Free $ TailRecC (map convert ops)
            | otherwise = convert expr

        tailRec _ other = convert other
-}

surfaceToTopLevel _ = Nothing
