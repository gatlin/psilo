{-# LANGUAGE DeriveFunctor #-}

module Test where

import Syntax
import Parser
import Typechecker
import Evaluator

import Control.Monad.Trans
import Control.Monad.State
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Free
import Control.Comonad

testsym = "okay"

dealloc :: Location -> Machine ()
dealloc loc = do
    sto <- gets mSto
    let sto' = IntMap.delete loc sto
    modify $ \st -> st { mSto = sto' }

-- Test load and store
m1 = runMachine newMachineState $ do
    store "five" $ VInteger 5
    five <- load "five"
    return five

-- Testing pushig and popping frames
m2 = runMachine newMachineState $ do
    loc1 <- fresh
    loc2 <- fresh
    update loc1 $ VInteger 1
    update loc2 $ VInteger 2
    bind "one" loc1
    bind "two" loc2
    st1 <- get
    liftIO . putStrLn . show $ st1
    dealloc loc1
    dealloc loc2
    (e1:e2:er) <- gets mEnv
    modify $ \st -> st { mEnv = er }
    st2 <- get
    liftIO . putStrLn . show $ st2
    return ()

t1 = runMachine newMachineState $ eval $ aInteger 5

thnk = VThunk (aInteger 5) []
m3 = runMachine newMachineState $ strict thnk

thnk2 = VThunk (Free (AApply (Free (ASymbol "+"))
                             [ Free (ASymbol "x"), Free (AInteger 1) ]))
               [("x", 1)]

m4 = runMachine ms $ strict thnk2
    where ms = MachineState [] (IntMap.fromList [(1, thnk )]) 2

vars = freeVariables (Free (ALambda ["x"]
                       (Free (AApply (Free (ASymbol "+"))
                                     [ Free (ASymbol "x")
                                     , Free (ASymbol "y") ]))))

ast_1 = parseTopLevel "(cons 3 (cons 2 (nil)))"
ast_2 = parseTopLevel "(cons 3 (cons 2 (cons 1 (nil))))"

cm_1 = cofreeMu ast_1' where Right (ast_1':_) = ast_1
cm_2 = cofreeMu ast_2' where Right (ast_2':_) = ast_2
