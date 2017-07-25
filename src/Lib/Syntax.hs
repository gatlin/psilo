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

import Lib.Types.Scheme
import Lib.Types.Qual
import Lib.Types.Type
import Lib.Types.Kind
import Lib.Types.TypeEnv (TypeEnv(..), generalize)

import Control.Monad.Free
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

-- | Converts a 'SurfaceExpr' to a 'CoreExpr' or fails.
-- TODO needs to convert each bound symbol into a unique symbol. Suggests a
-- reader monad.
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


surfaceToTopLevel (Free (SigS sym vars (preds, ty))) = Just $ Signature sym schm
    where

        tyVarMap :: Map Symbol Int
        tyVarMap = M.fromList (zip vars [0..])

        schm :: Scheme
        schm = generalize mempty qualType

        qualType :: Qual Type
        qualType = (convertPreds [] preds) :=> (convertTypes ty)

        convertPreds :: [Pred] -> [(Symbol, Symbol)] -> [Pred]
        convertPreds result [] = result
        convertPreds result ((p, t):ps) =
            convertPreds
            ((IsIn p (TVar (TyVar (tyVarMap M.! t) Star))):result)
            ps

        convertTypes :: [[Symbol]] -> Type
        convertTypes tys
            | length tys == 1 = convertType (tys !! 0)
            | otherwise = TFun (fmap convertType tys)

        convertType :: [Symbol] -> Type
        convertType t
            | length t == 1 =
                  let s = t !! 0
                  in  case M.lookup s tyVarMap of
                          Nothing -> TSym (TyCon (t !! 0) Star)
                          Just n  -> TVar (TyVar n Star)
