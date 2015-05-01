module Test where

import Syntax
import Parser
import Typechecker
import Evaluator

import Control.Monad.Trans
import Data.Maybe

testsym = "okay"

load :: Symbol -> Machine (Maybe Value)
load sym = do
    maybeLoc <- query sym
    case maybeLoc of
        Just loc -> fetch loc >>= return
        Nothing  -> return Nothing

store :: Symbol -> Value -> Machine ()
store sym val = do
    loc <- fresh
    update loc val
    pushFrame [(sym, loc)]

m1 = do
    store "five" $ VInteger 5
    five <- load "five"
    return five
