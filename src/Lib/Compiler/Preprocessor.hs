{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler.Preprocessor where

import Lib.Syntax.Symbol
import Lib.Syntax.Surface
import Lib.Syntax.Core

import Lib.Types.Type (TyVar(..), Type(..))
import Lib.Types.Qual
import Lib.Types.TypeEnv
import Lib.Types.Scheme
import Lib.Types.Class
import Lib.Types (typecheck_defns)

import Lib.Errors

import Control.Monad (forM)

import Control.Monad.Free
import Control.Comonad.Cofree

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as M

-- | The preprocessor performs syntactic transformations on 'SurfaceExpr's.
newtype Preprocess m a = Preprocess {
    unPreprocess
        :: StateT Int (ReaderT (Map Symbol Symbol) (ExceptT PsiloError m)) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState Int
               , MonadReader (Map Symbol Symbol)
               , MonadError PsiloError
               )

preprocess
    :: (Monad m)
    => Preprocess m a
    -> ExceptT PsiloError m a
preprocess (Preprocess p) = runReaderT (evalStateT p 0) M.empty

gensym :: Monad m => Preprocess m String
gensym = do
    n <- get
    modify $ \n -> n + 1
    return $ "-" ++ (show n)

readBoundVars :: Monad m => Preprocess m (Map Symbol Symbol)
readBoundVars = ask

withBoundVars
    :: Monad m
    => (Map Symbol Symbol)
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

uniqueIds (Free (SigS sym vars pt)) = do
    boundVars <- readBoundVars
    let sym' = maybe sym id $ M.lookup sym boundVars
    return $ aSig sym' vars pt

uniqueIds whatever = return whatever
