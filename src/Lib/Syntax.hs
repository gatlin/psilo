module Lib.Syntax
    ( Symbol
    , builtin_syms
    , CoreExpr
    , CoreAst(..)
    , Definition(..)
    , SurfaceExpr
    , SurfaceAst(..)
    , surfaceToCore
    , surfaceToDefinition
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

import Control.Monad.Free

-- | Converts a 'SurfaceExpr' to a 'CoreExpr' or fails
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

-- | Converts a 'SurfaceExpr' to a 'Definition' or fails
surfaceToDefinition :: SurfaceExpr () -> Maybe Definition
surfaceToDefinition (Free (DefS sym expr)) = do
    core <- surfaceToCore expr
    return $ Define sym core
{-
        tailRec sym (Free (IfS c t e)) = Free $
            IfC (convert c) (tailRec sym t) (tailRec sym e)

        tailRec sym expr@(Free (AppS (Free (IdS fun)) ops))
            | sym == fun = Free $ TailRecC (map convert ops)
            | otherwise = convert expr

        tailRec _ other = convert other
-}

surfaceToDefinition _ = Nothing
