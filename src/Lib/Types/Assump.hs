module Lib.Types.Assump
where

import Lib.Syntax (Symbol)
import Lib.Types.Scheme
import Lib.Types.Subst

data Assump = Symbol :>: Scheme

instance Show Assump where
    show (i :>: s) = show i ++ " :>: " ++ show s

instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)

    tv (i :>: sc) = tv sc

find :: Monad m => Symbol -> [Assump] -> m Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((i' :>: sc):as) = if i == i' then return sc else find i as
