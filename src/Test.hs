{-# LANGUAGE DeriveFunctor #-}

module Test where

import Syntax
import Parser
import Evaluator
import Typechecker

import Control.Monad.Trans
import Control.Monad.State
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Free
import Control.Comonad

testsym = "okay"

getAst x = case parseTopLevel x of
    Left err -> error $ "wtf: " ++ (show err)
    Right (a:_)  -> a

square_ast = getAst "(= square (x) (* x x))"
square_mu  = cofreeMu square_ast
