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

getAst x = let Right (a:_) = parseTopLevel x in a

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

add_ast = getAst "(\\ (x y) (+ x y))"
mul_ast = getAst "((\\ (x y) (* x y)) 5 3)"
foo_ast = getAst "(foo x y z w b)"

toUnary :: Expr () -> Expr ()
toUnary (Free (ALambda [] body)) = Free $ ALambda [] $ toUnary body
toUnary (Free (ALambda (a:[]) body)) = Free $ ALambda [a] $ toUnary body
toUnary (Free (ALambda (a:as) body)) = Free $ ALambda [a] $ toUnary $ Free $ ALambda as body

toUnary (Free (AApply op [])) = Free $ AApply (toUnary op) []
toUnary (Free (AApply op (e:[]))) = Free $ AApply (toUnary op) [toUnary e]
toUnary (Free (AApply op es)) = Free $ AApply op' [toUnary e_last]
    where e_init = init es
          e_last = last es
          op'    = Free $ AApply op e_init

toUnary e = e
