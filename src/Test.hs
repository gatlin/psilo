module Test where

import Syntax
import Parser
import Typechecker
import Evaluator

import Control.Monad.Trans
import Control.Monad.State
import Data.Maybe

testsym = "okay"

-- Test load and store
m1 = do
    store "five" $ VInteger 5
    five <- load "five"
    return five

-- Testing pushig and popping frames
m2 = do
    loc1 <- fresh
    loc2 <- fresh
    update loc1 $ VInteger 1
    update loc2 $ VInteger 2
    pushFrame [("one", loc1), ("two", loc2)]
    st1 <- get
    liftIO . putStrLn . show $ st1
    popFrame
    st2 <- get
    liftIO . putStrLn . show $ st2
    return ()


