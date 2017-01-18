module Lib.Interpreter where

import Lib.Runtime
import Lib.Syntax
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad (forM, forM_, filterM)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

import Prelude hiding (lookup)

-- * Builtin operations and symbols

op_plus :: Value -> Value -> Value
op_plus (NumV l) (NumV r) = NumV $ l + r
op_plus _        _        = error "Can't add non-numbers"

op_mult :: Value -> Value -> Value
op_mult (NumV l) (NumV r) = NumV $ l * r
op_mult _        _        = error "Can't multiply non-numbers"

op_minus :: Value -> Value -> Value
op_minus (NumV l) (NumV r) = NumV $ l - r
op_minus _ _ = error "Can't subtract non-numbers"

op_div :: Value -> Value -> Value
op_div (NumV l) (NumV r) = NumV $ l / r
op_div _ _ = error "Can't divide non-numbers"

op_eq :: Value -> Value -> Value
op_eq (BoolV l) (BoolV r) = BoolV $ l == r
op_eq (NumV l) (NumV r)   = BoolV $ l == r
op_eq _         _         = error "Values must be both numeric or boolean"

op_lt :: Value -> Value -> Value
op_lt (NumV l) (NumV r) = BoolV $ l < r

op_gt :: Value -> Value -> Value
op_gt (NumV l) (NumV r) = BoolV $ l > r

builtin_syms :: Set Symbol
builtin_syms = S.fromList
    [ "+", "*", "-", "/", "=", "<", ">" ]

-- | Given an expression produce a set of free variable symbols
free_variables :: CoreExpr () -> Set Symbol -> Set Symbol
free_variables expr tlds = (flip runReader) tlds $ go expr S.empty where
    go :: CoreExpr () -> Set Symbol -> Reader (Set Symbol) (Set Symbol)
    go (Free (NumC _)) fvs = return fvs
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
make_closure :: Set Symbol -> [Symbol] -> CoreExpr () -> Runtime Value
make_closure captured args body = case S.null captured of
    True -> return $ ClosV args body emptyEnv
    False -> do
        capturedVars <- filterM (fmap not . isSymTopLevel) $
                        filter (\s -> not (S.member s builtin_syms)) $
                                  S.toList captured
        cEnv <- forM capturedVars $ \fv -> do
            Just oldLoc <- lookup fv
            newLoc <- nextLoc
            val <- duplicateInStore oldLoc newLoc
            return $ bind fv newLoc
        return $ ClosV args body cEnv

-- | Interpret an 'CoreExpr ()' in a 'Runtime' to produce a result 'Value'
interpret :: CoreExpr () -> Runtime Value
interpret (Free (NumC n)) = return $ NumV n
interpret (Free (BoolC b)) = return $ BoolV b
interpret (Free (StringC s)) = return $ StringV s
interpret (Free (IdC s)) = do
    case S.member s builtin_syms of
        True -> return $ SymV s
        False -> do
            mSym <- lookupAndFetch s
            case mSym of
                Nothing -> error $ "Unbound symbol: " ++ s
                Just val -> return val

interpret (Free (IfC c t e)) = do
    BoolV condVal <- interpret c
    if condVal
        then interpret t
        else interpret e

interpret clos@(Free (ClosC args body)) = do
    tlds <- gets topLevelDefns
    let freeVars = free_variables clos $ M.keysSet tlds
    make_closure freeVars args body

interpret app@(Free (AppC fun appArgs)) = do
    funV <- interpret fun
    (locs, vals) <- (forM appArgs $ \arg -> do
        resultVal <- interpret arg
        loc <- nextLoc
        overrideStore loc resultVal
        return (loc, resultVal)) >>= return . unzip
    result <- case funV of
        ClosV closArgs body cEnv -> do
            let bindings = zipWith bind closArgs locs
            result <- local (\env -> bindings ++ cEnv ++ env) $
                interpret body
            -- now remove that shit from the store
            forM_ cEnv $ \(Binding _ loc) -> removeFromStore loc
            return result
        SymV sym -> case sym of
            "+" -> return $ op_plus (vals !! 0) (vals !! 1)
            "*" -> return $ op_mult (vals !! 0) (vals !! 1)
            "-" -> return $ op_minus (vals !! 0) (vals !! 1)
            "/" -> return $ op_div (vals !! 0) (vals !! 1)
            "=" -> return $ op_eq (vals !! 0) (vals !! 1)
            "<" -> return $ op_lt (vals !! 0) (vals !! 1)
            ">" -> return $ op_gt (vals !! 0) (vals !! 1)

    forM_ locs $ \loc -> removeFromStore loc
    return result

interpret (Free (DefC sym body)) = do
    tlds <- gets topLevelDefns
    loc <- nextLoc
    modify $ \st -> st { topLevelDefns = M.insert sym loc tlds }
    bodyVal <- interpret body
    overrideStore loc bodyVal
    return NopV
