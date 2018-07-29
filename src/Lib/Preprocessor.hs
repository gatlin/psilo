{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- This module facilitates turning parsed surface expressions into the data
-- needed by the compiler to generate code. These tasks include:
-- [x] Generating unique identifiers for every distinct variable
-- [x] Converting surface-level definitions into TopLevel
-- [x] Converting surface-level signatures into TopLevel
-- [x] Ensuring all symbols are either bound or top level
-- [x] Forming a default type environment for type checking
-- [ ] Gathering predicate information to form a class environment
-- [ ] Lambda lifting

module Lib.Preprocessor where

import           Lib.Compiler

import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Surface
import           Lib.Syntax.Symbol
import           Lib.Syntax.TopLevel
import           Lib.Types.Frame
import           Lib.Types.Kind         (Kind (..))
import           Lib.Types.Scheme
import           Lib.Types.Type
import           Lib.Types.TypeCheck

import           Lib.Errors

import           Control.Comonad.Cofree
import           Control.Monad          (forM, mapM, mapM_, when)
import           Control.Monad.Free

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans

import           Data.Map               (Map)
import qualified Data.Map               as M

import           Data.Set               (Set)
import qualified Data.Set               as S

type SymbolMap = Map Symbol Symbol
data PState = PState
    { uniqueInt :: Int -- ^ For generating unique IDs
    }

pState :: PState
pState = PState 0

-- | A monad for transforming parsed surface syntax into what the compiler needs
newtype Preprocess a = Preprocess {
    runPreprocess
        :: StateT PState (ReaderT SymbolMap Compiler) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState PState
               , MonadReader (Map Symbol Symbol)
               , MonadError PsiloError
               )

preprocess
    :: Preprocess a
    -> Compiler a
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
    case compile (annotated val') of
        Left _    -> throwError $ PreprocessError "wut"
        Right ann -> return $ mempty {
            definitions = M.singleton sym ann
            }
--        Right ann -> return [ Define sym ann ]

surfaceToTopLevel (Free (SigS sym scheme)) = return $ mempty {
    signatures = M.singleton sym (normalize $ quantify scheme)
    }
--    [ Signature sym $ normalize $ quantify scheme ]

surfaceToTopLevel (Free (TypedefS name vars body)) = do
    let body' = normalize . quantify $ body
    let ret_type = TList $ (TSym (TyLit name Star)) : (fmap TVar vars)
    let ctor = normalize $ TForall vars $ TList $
               tyFun : (body : [ret_type])
    let dtor' = normalize $ TForall vars $ TList $
               tyFun : (ret_type : [body])
    let mDtor = runSolve (skolemize dtor') initTypeCheckState
    dtor <- case mDtor of
        Left err              -> throwError err
        Right (sk_vars, dtor) -> return $ TForall sk_vars dtor

    return $ mempty {
        typedefs = M.singleton name (vars, normalize $ quantify body),
        signatures = M.fromList [(name, ctor), (('~':name), dtor)]
        }
{-
    return
        [ Typedef name vars $ normalize $ quantify body
        , Signature name ctor
        , Signature ("~"++name) dtor
        ]
-}

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

fst' :: (a, b, c) -> a
fst' (x, y, z) = x

snd' :: (a, b, c) -> b
snd' (x, y, z) = y

thd' :: (a, b, c) -> c
thd' (x, y, z) = z

-- | Ensures that all symbols are either bound or global
-- TODO why am I not using a Set here?
boundVarCheck :: TopLevel -> Preprocess ()
boundVarCheck (TopLevel defns sigs tds) =
    withBoundVars bvs $ mapM_ go $ M.toList defns
    where
        dup x = (x, x)
        syms = defn_syms ++
               (fmap dup ty_defn_syms) ++
               (fmap dup (fmap ("~"++) ty_defn_syms))
        defn_syms = fmap dup $ M.keys defns
        ty_defn_syms = M.keys tds
        builtins = fmap dup $ S.toList builtin_syms
        bvs = M.fromList $ builtins ++ syms

        go :: (Symbol, AnnotatedExpr ()) -> Preprocess ()
        go (s, core) = check core

        check :: AnnotatedExpr () -> Preprocess ()
        check (() :< (FunC args body)) = do
            let argSyms = fmap (\x -> (x, x)) args
            b' <- withBoundVars (M.fromList argSyms) $ check body
            return ()

        check (() :< (IdC s)) = do
            boundVars <- readBoundVars
            when (M.notMember s boundVars) $
                throwError $ UnboundVariable s
            return ()

        check (() :< (AppC op erands)) = do
            op' <- check op
            erands' <- mapM check erands
            return ()

        check (() :< (IfC c t e)) = do
            c' <- check c
            t' <- check t
            e' <- check e
            return ()

        check whatever = return ()
