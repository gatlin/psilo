module Lib.Util where

import Prelude hiding (lookup)
import Lib.Syntax
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid
import Control.Monad.Free
import Control.Monad.Reader

builtin_syms :: Set Symbol
builtin_syms = S.fromList
    [ "+", "*", "-", "/", "=", "<", ">" ]

-- | Given an expression produce a set of free variable symbols
free_variables :: CoreExpr () -> Set Symbol -> Set Symbol
free_variables expr tlds = (flip runReader) tlds $ go expr S.empty where
    go :: CoreExpr () -> Set Symbol -> Reader (Set Symbol) (Set Symbol)
    go (Free (IntC _)) fvs = return fvs
    go (Free (DoubleC _)) fvs = return fvs
    go (Free (BoolC _)) fvs = return fvs
    go (Free (StringC _)) fvs = return fvs
    go (Free (IfC c t e)) fvs = do
        c_fvs <- go c fvs
        t_fvs <- go t c_fvs
        e_fvs <- go e t_fvs
        return e_fvs
    go (Free (IdC sym)) fvs = do
        boundVars <- ask
        case S.member sym (S.union builtin_syms boundVars) of
            True -> return fvs
            False -> return $ S.insert sym fvs
    go (Free (AppC fun args)) fvs = do
        fun_freeVars <- go fun fvs
        args' <- forM args $ \arg ->
            go arg fvs
        let arg_freeVars = foldl S.union S.empty args'
        return $ S.union fun_freeVars arg_freeVars
    go (Free (ClosC args body)) fvs =
        local (\boundVars ->
                   foldl (\bvs arg -> S.insert arg bvs) boundVars args) $
          go body fvs

-- | Given a set of captured environment symbols and the pieces of a lambda
-- abstraction expression, duplicate the relevant portions of the store and
-- produce a new closure value with its own environment
{-
make_closure :: Set Symbol -> [Symbol] -> CoreExpr () -> Runtime Value
make_closure captured args body = case S.null captured of
    True -> return $ ClosV args body emptyEnv
    False -> do
        capturedVars <- filterM (fmap not . isSymTopLevel) $
                        filter (\s -> not (S.member s builtin_syms)) $
                                  S.toList captured
        cEnv <- forM capturedVars $ \fv -> do
            Just oldLoc <- lookup fv
            mOldLoc <- lookup fv
            case mOldLoc of
                Just oldLoc -> do
                    newLoc <- nextLoc
                    val <- duplicateInStore oldLoc newLoc
                    return $ bind fv newLoc
                Nothing -> error $ "Looking up " ++ (show fv)
        return $ ClosV args body $ envFromBindings cEnv


compile :: CoreExpr () -> [ Asm ]
compile expr = reverse (go expr []) where
    go (Pure _) asm = asm
    go (Free (NumC n)) asm = (Push n) : asm
    go (Free (BoolC b)) asm = (Push (if b then 1 else 0)) : asm
--    go (Free (StringC str)) asm =
-}

-- | Interpret an 'CoreExpr ()' in a 'Runtime' to produce a result 'Value'
