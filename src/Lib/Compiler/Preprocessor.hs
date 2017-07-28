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

import Control.Monad (forM)

import Control.Monad.Free
import Control.Comonad.Cofree

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as M

-- | Errors one might encounter while preprocessing
data PreprocessError
    = OtherPreprocessError

-- | The preprocessor performs syntactic transformations on 'SurfaceExpr's.
newtype Preprocess a = Preprocess {
    unPreprocess :: StateT Int
                    (ReaderT (Map Symbol Symbol)
                    (Except PreprocessError)) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState Int
               , MonadReader (Map Symbol Symbol)
               , MonadError PreprocessError
               )

runPreprocess :: Preprocess a -> Either PreprocessError a
runPreprocess (Preprocess p) = runExcept (runReaderT (evalStateT p 0) M.empty)

gensym :: Preprocess String
gensym = do
    n <- get
    modify $ \n -> n + 1
    return $ "-" ++ (show n)

readBoundVars :: Preprocess (Map Symbol Symbol)
readBoundVars = ask

withBoundVars :: (Map Symbol Symbol) -> Preprocess a -> Preprocess a
withBoundVars bvs m = local (M.union bvs) m

-- | Give each symbol a globally unique identifier
uniqueIds :: SurfaceExpr () -> Preprocess (SurfaceExpr ())
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
    return . join $ aFun uniqueVars body' sigs

uniqueIds (Free (AppS op erands)) = do
    op' <- uniqueIds op
    erands' <- forM erands uniqueIds
    return . join $ aApp op' erands'

uniqueIds (Free (IfS c t e)) = do
    c' <- uniqueIds c
    t' <- uniqueIds t
    e' <- uniqueIds e
    return . join $ aIf c' t' e'

uniqueIds (Free (DefS sym val)) = do
    val' <- uniqueIds val
    return . join $ aDef sym val'

uniqueIds whatever = return whatever
