{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- This module facilitates turning parsed surface expressions into the data
-- needed by the compiler to generate code. These tasks include:
-- [x] Generating unique identifiers for every distinct variable
-- [x] Converting surface-level definitions into TopLevel
-- [x] Converting surface-level signatures into TopLevel
-- [x] Ensuring all symbols are either bound or top level
-- [ ] Forming a default type environment for type checking
-- [ ] Gathering predicate information to form a class environment
-- [ ] Lambda lifting

module Lib.Preprocessor where

import Lib.Syntax.Symbol
import Lib.Syntax.Surface
import Lib.Syntax.Core
import Lib.Syntax.Annotated
import Lib.Syntax.TopLevel

import Lib.Types.Type (TyVar(..), Type(..))
import Lib.Types.Qual
import Lib.Types.TypeEnv
import Lib.Types.Scheme
import Lib.Types.Class
import Lib.Types (typecheck_defns)

import Lib.Util

import Lib.Errors

import Control.Monad (forM, mapM, mapM_, when)
import Control.Monad.Free
import Control.Comonad.Cofree

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

type SymbolMap = Map Symbol Symbol
data PState = PState
    { uniqueInt :: Int -- ^ For generating unique IDs
    }

pState :: PState
pState = PState 0

-- | A monad for transforming parsed surface syntax into what the compiler needs
newtype Preprocess a = Preprocess {
    runPreprocess
        :: StateT PState (ReaderT SymbolMap (Except PsiloError)) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState PState
               , MonadReader (Map Symbol Symbol)
               , MonadError PsiloError
               )

preprocess
    :: Preprocess a
    -> Except PsiloError a
preprocess (Preprocess p) = runReaderT (evalStateT p pState ) M.empty

-- | Generate unique symbols
gensym :: Preprocess String
gensym = do
    n <- gets uniqueInt
    modify $ \s -> s { uniqueInt = n + 1 }
    return $ "_" ++ (show n)

readBoundVars :: Preprocess SymbolMap
readBoundVars = ask

-- | Perform a preprocessing computation with a temporarily extended bound
-- variable map
withBoundVars
    :: SymbolMap
    -> Preprocess a
    -> Preprocess a
withBoundVars bvs m = local (M.union bvs) m

-- | Give each symbol in a 'SurfaceExpr' a globally unique identifier
uniqueIds
    :: SurfaceExpr ()
    -> Preprocess (SurfaceExpr ())

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

-- | Transforms a 'SurfaceExpr' into a 'TopLevel' expression
surfaceToTopLevel
    :: SurfaceExpr ()
    -> Preprocess TopLevel
surfaceToTopLevel (Free (DefS sym val)) = do
    uval <- uniqueIds val
    val' <- surfaceToCore uval
    return $ Define sym val'

surfaceToTopLevel (Free (SigS sym scheme)) = return $
    Signature sym $ normalize scheme

surfaceToTopLevel _ = throwError $
    PreprocessError $
    "Expression is not a top level expression"

-- | Called by 'surfaceToTopLevel' on a subset of 'SurfaceExpr's
surfaceToCore
    :: SurfaceExpr ()
    -> Preprocess (CoreExpr ())
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

-- | Ensures that all symbols are either bound or global
boundVarCheck :: [TopLevel] -> Preprocess ()
boundVarCheck toplevels = withBoundVars bvs $ mapM_ go toplevels
    where
        syms = fmap (\x -> (x,x)) $ fmap fst $ fst $ splitUp toplevels
        builtins = fmap (\x -> (x,x)) $ S.toList builtin_syms
        bvs = M.fromList $ builtins ++ syms

        go :: TopLevel -> Preprocess ()
        go (Define s core) = check core
        go whatever = return ()

        check :: CoreExpr () -> Preprocess ()
        check (Free (FunC args body)) = do
            let argSyms = fmap (\x -> (x, x)) args
            b' <- withBoundVars (M.fromList argSyms) $ check body
            return ()

        check (Free (IdC s)) = do
            boundVars <- readBoundVars
            when (M.notMember s boundVars) $
                throwError $ UnboundVariable s
            return ()

        check (Free (AppC op erands)) = do
            op' <- check op
            erands' <- mapM check erands
            return ()

        check (Free (IfC c t e)) = do
            c' <- check c
            t' <- check t
            e' <- check e
            return ()

        check whatever = return ()
