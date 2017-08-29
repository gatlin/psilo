{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- This module facilitates turning parsed surface expressions into the data
-- needed by the compiler to generate code. These tasks include:
-- - Generating unique identifiers for every distinct variable
-- - Converting surface expressions into top level expressions
--   (core expressions, type schemes, etc)
-- - Lambda lifting

module Lib.Preprocessor where

import Lib.Syntax.Symbol
import Lib.Syntax.Surface
import Lib.Syntax.Core
import Lib.Syntax.TopLevel

import Lib.Types.Type (TyVar(..), Type(..))
import Lib.Types.Qual
import Lib.Types.TypeEnv
import Lib.Types.Scheme
import Lib.Types.Class
import Lib.Types (typecheck_defns)

import Lib.Errors

import Control.Monad (forM, mapM)
import Control.Monad.Free
import Control.Comonad.Cofree

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as M

type SymbolMap = Map Symbol Symbol
data PState = PState
    { uniqueInt :: Int -- ^ For generating unique IDs
    }

pState :: PState
pState = PState 0

-- | A monad for transforming parsed surface syntax into what the compiler needs
newtype Preprocess m a = Preprocess {
    runPreprocess
        :: StateT PState (ReaderT SymbolMap (ExceptT PsiloError m)) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState PState
               , MonadReader (Map Symbol Symbol)
               , MonadError PsiloError
               )

preprocess
    :: (Monad m)
    => Preprocess m a
    -> ExceptT PsiloError m a
preprocess (Preprocess p) = runReaderT (evalStateT p pState ) M.empty

gensym :: Monad m => Preprocess m String
gensym = do
    n <- gets uniqueInt
    modify $ \s -> s { uniqueInt = n + 1 }
    return $ "-" ++ (show n)

readBoundVars :: Monad m => Preprocess m SymbolMap
readBoundVars = ask

withBoundVars
    :: Monad m
    => SymbolMap
    -> Preprocess m a
    -> Preprocess m a
withBoundVars bvs m = local (M.union bvs) m

-- | Give each symbol a globally unique identifier
uniqueIds
    :: Monad m
    => SurfaceExpr ()
    -> Preprocess m (SurfaceExpr ())

uniqueIds (Free (IdS s)) = do
    boundVars <- readBoundVars
    case M.lookup s boundVars of
        Nothing -> return $ aId s
        Just s' -> return $ aId s'

-- | add new scope of bound variables to the environment
uniqueIds (Free (FunS args body sigs)) = do
    boundVars <- readBoundVars
    uniqueVars <- forM args $ \arg -> do
        suffix <- gensym
        return $ arg ++ suffix
    let bvs = M.fromList $ zip args uniqueVars
    body' <- withBoundVars bvs $ uniqueIds body
    sigs' <- withBoundVars bvs $ forM sigs $ \sig -> do
        sig' <- sequence $ fmap uniqueIds sig
        return sig'
    return $ aFun uniqueVars body' sigs'

uniqueIds (Free (AppS op erands)) = do
    op' <- uniqueIds op
    erands' <- forM erands uniqueIds
    return $ aApp op' erands'

uniqueIds (Free (IfS c t e)) = do
    c' <- uniqueIds c
    t' <- uniqueIds t
    e' <- uniqueIds e
    return $ aIf c' t' e'

uniqueIds (Free (DefS sym val)) = do
    val' <- uniqueIds val
    return $ aDef sym val'

uniqueIds (Free (SigS sym pt)) = do
    boundVars <- readBoundVars
    let sym' = maybe sym id $ M.lookup sym boundVars
    return $ aSig sym' pt

uniqueIds whatever = return whatever

surfaceToTopLevel
    :: Monad m
    => SurfaceExpr ()
    -> Preprocess m TopLevel
surfaceToTopLevel s@(Free (DefS sym val)) = do
    s' <- uniqueIds s
    val' <- surfaceToCore val
    return $ Define sym val'

{-
    | SigS { sigSymS :: Symbol
           , sigVarS :: [TypeLit]
           , sigTypeS :: ([(Symbol, TypeLit)], [[TypeLit]]) }

data TypeLit = TyConLit String | TyVarLit String
    deriving (Eq, Ord, Show)

data Scheme = Forall [TyVar] (Qual Type) deriving (Eq, Ord)
-}
surfaceToTopLevel s@(Free (SigS sym scheme)) =
    return $ TopLevelNull (show s)

surfaceToTopLevel _ = throwError $
    PreprocessError $
    "Expression is not a top level expression"

surfaceToCore
    :: Monad m
    => SurfaceExpr ()
    -> Preprocess m (CoreExpr ())
surfaceToCore (Free (IntS n)) = return $ cInt n
surfaceToCore (Free (FloatS n)) = return $ cFloat n
surfaceToCore (Free (BoolS b)) = return $ cBool b
surfaceToCore (Free (IdS s)) = return $ cId s
surfaceToCore (Free (AppS op erands)) = do
    op' <- surfaceToCore op
    erands' <- mapM surfaceToCore erands
    return $ cApp op' erands'

surfaceToCore (Free (FunS a b _)) = do
    b' <- surfaceToCore b
    return $ cFun a b'

surfaceToCore (Free (IfS c t e)) = do
    c' <- surfaceToCore c
    t' <- surfaceToCore t
    e' <- surfaceToCore e
    return $ cIf c' t' e'

surfaceToCore s = throwError $ PreprocessError $
    "Expression " ++ show s ++ " cannot be converted into a core expression."
