{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- This module facilitates turning parsed surface expressions into the data
-- needed by the compiler to generate code. These tasks include:
-- [x] Generating unique identifiers for every distinct variable
-- [x] Converting surface-level definitions into TopLevel
-- [x] Converting surface-level signatures into TopLevel
-- [x] Ensuring all symbols are either bound or top level
-- [x] Forming a default type environment for type checking

module Lib.Preprocessor where

import           Lib.Compiler

import           Lib.Syntax
import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Surface
import           Lib.Syntax.Symbol
import           Lib.Syntax.TopLevel
import           Lib.Types.Class
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

import           Data.Monoid            ((<>))

type SymbolMap = Map Symbol Symbol
data PState = PState
    { uniqueInt :: Int -- ^ For generating unique IDs
    , toplevel  :: TopLevel
    }

pState :: PState
pState = PState 0 mempty

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
    -> Compiler TopLevel
preprocess (Preprocess p) = do
    (PState _ toplevel ) <- runReaderT (execStateT p pState ) M.empty
    return toplevel

-- | Generate unique symbols
gensym :: Preprocess String
gensym = do
    n <- gets uniqueInt
    modify $ \s -> s { uniqueInt = n + 1 }
    return $ "_" ++ (show n)

genint :: Preprocess Int
genint = do
    n <- gets uniqueInt
    modify $ \s -> s { uniqueInt = n + 1 }
    return n

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

addDefinition :: Symbol -> AnnotatedExpr (Maybe Type) -> Preprocess ()
addDefinition sym dfn = do
    tl <- gets toplevel
    let tl' = tl {
            definitions = M.insert sym dfn (definitions tl)
            }
    modify $ \st -> st { toplevel = tl' }

lookupDefinition :: Symbol -> Preprocess (Maybe (AnnotatedExpr (Maybe Type)))
lookupDefinition sym = do
    tl <- gets toplevel
    return $ M.lookup sym (definitions tl)

addSignature :: Symbol -> Sigma -> Preprocess ()
addSignature sym sig = do
    tl <- gets toplevel
    let sig' = normalize $ quantify sig
    let tl' = tl {
            signatures = M.insert sym sig' (signatures tl)
            }
    modify $ \st -> st { toplevel = tl' }

lookupSignature :: Symbol -> Preprocess (Maybe Sigma)
lookupSignature sym = do
    tl <- gets toplevel
    return $ M.lookup sym (signatures tl)

addTypedef :: Symbol -> ([TyVar], Sigma, Bool) -> Preprocess ()
addTypedef sym td = do
    tl <- gets toplevel
    let tl' = tl {
            typedefs = M.insert sym td (typedefs tl)
            }
    modify $ \st -> st { toplevel = tl' }

lookupTypedef :: Symbol -> Preprocess (Maybe ([TyVar], Sigma, Bool))
lookupTypedef sym = do
    tl <- gets toplevel
    return $ M.lookup sym (typedefs tl)

addClassdef :: Symbol -> [Type] -> [Pred] -> Preprocess ()
addClassdef sym vars preds = do
    tl <- gets toplevel
    let tl' = tl {
            classes = (classes tl) <:> (addClass sym vars preds)
            }

    modify $ \st -> st { toplevel = tl' }

addMethod
    :: Symbol
    -> (Set (AnnotatedExpr (Maybe Type)))
    -> Preprocess ()
addMethod sym mthds = do
    tl <- gets toplevel
    let ms = case M.lookup sym (methods tl) of
                  Nothing -> M.insert sym mthds (methods tl)
                  Just mthds' ->
                      M.insert sym (mthds `S.union` mthds') (methods tl)
    let tl' = tl {
            methods = ms
            }
    modify $ \st -> st { toplevel = tl' }

makeDefinition :: SurfaceExpr () -> Preprocess (AnnotatedExpr (Maybe Type))
makeDefinition (Free (DefS sym val)) = do
    uval <- uniqueIds val >>= surfaceToCore
    case compile (annotated uval) of
        Left _ -> throwError $ PreprocessError $ "Error desugaring " ++ sym
        Right ann -> return $ fmap (const Nothing) ann

makeDefinition _ = throwError $ PreprocessError "Not a valid definition."

-- | Transforms a 'SurfaceExpr' into a 'TopLevel' expression
-- In general, a surface level syntax object might generate multiple types of
-- "top level" declarations; eg, a class definition generates signatures and
-- potential method implementations as well as the actual class info.
surfaceToTopLevel
    :: SurfaceExpr ()
    -> Preprocess ()
surfaceToTopLevel  d@(Free (DefS sym val)) = do
    dfn <- makeDefinition d
    addDefinition sym dfn

surfaceToTopLevel  (Free (SigS sym sig)) = addSignature sym sig

-- Generates signatures for constructor and destructor, as well as a proper
-- typedef object.
surfaceToTopLevel  (Free (TypeDefS name vars body isAlias)) = do
    let body' = quantify body
    addTypedef name (vars, body', isAlias)
    when (not isAlias) $ do
        let ret_type = TList $ (TSym (TyLit name Star)) : (fmap TVar vars)
        let ctor = TForall vars $ TList $
                   tyFun : (body : [ret_type])
        let dtor' = TForall vars $ TList $
                    tyFun : (ret_type : [body])
        let mDtor = runSolve (skolemize dtor') initTypeCheckState
        dtor <- case mDtor of
            Left err              -> throwError err
            Right (sk_vars, dtor) -> return $ TForall sk_vars dtor
        addSignature name ctor
        addSignature ('~':name) dtor

surfaceToTopLevel (Free (ClassDefS name vars preds mthods)) = do
    addClassdef name vars preds
    forM_ mthods $ \(Free (SigS sym scheme), mDfn) -> do
        let sig = quantify $ qualify [TPred name vars] scheme
        addSignature sym sig
        case mDfn of
            Nothing -> addMethod sym S.empty
            Just d@(Free (DefS sym val)) -> do
                dfn' <- makeDefinition d
                let s = S.fromList [dfn']
                addMethod sym s

surfaceToTopLevel (Free (ClassInstS name vars preds mthods)) = do
    forM_ mthods $ \d@(Free (DefS sym val)) -> do
        dfn <- makeDefinition d
        mSig <- lookupSignature sym
        case mSig of
            Nothing  -> throwError $ PreprocessError "Not a class method."
            Just sig -> addMethod sym $ S.fromList [dfn]

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
-- TODO why am I not using a Set here?
boundVarCheck :: TopLevel -> Preprocess ()
boundVarCheck (TopLevel defns sigs tds cls mthds) =
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

        go :: (Symbol, AnnotatedExpr (Maybe Type)) -> Preprocess ()
        go (s, core) = check core

        check :: AnnotatedExpr (Maybe Type) -> Preprocess ()
        check (ty :< (FunC args body)) = do
            let argSyms = fmap (\x -> (x, x)) args
            b' <- withBoundVars (M.fromList argSyms) $ check body
            return ()

        check (_ :< (IdC s)) = do
            boundVars <- readBoundVars
            when (M.notMember s boundVars) $ do
                case M.lookup s sigs of
                    Nothing -> throwError $ UnboundVariable s
                    Just _  -> return ()
            return ()

        check (_ :< (AppC op erands)) = do
            op' <- check op
            erands' <- mapM check erands
            return ()

        check (_ :< (IfC c t e)) = do
            c' <- check c
            t' <- check t
            e' <- check e
            return ()

        check whatever = return ()
