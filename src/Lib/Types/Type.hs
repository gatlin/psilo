module Lib.Types.Type
where

import Lib.Syntax (Symbol)
import Lib.Types.Kind

data Type
    = TVar Tyvar
    | TCon Tycon
    | TAp Type Type
    | TGen Int
    deriving (Eq)

instance Show Type where
    show (TVar tyvar) = show tyvar
    show (TCon tycon) = show tycon
    show (TAp a b) = "( " ++ show a ++ " " ++ show b ++ " )"
    show (TGen n) = "n" ++ show n

data Tyvar = Tyvar Symbol Kind
    deriving (Eq)

instance Show Tyvar where
    show (Tyvar sym _) = "t"++sym

data Tycon = Tycon Symbol Kind
    deriving (Eq)

instance Show Tycon where
    show (Tycon sym _) = sym

class HasKind t where
    kind :: t -> Kind

instance HasKind Tyvar where
    kind (Tyvar _ k) = k

instance HasKind Tycon where
    kind (Tycon _ k) = k

instance HasKind Type where
    kind (TCon tc) = kind tc
    kind (TVar u) = kind u
    kind (TAp t _) = case (kind t) of
                         (Kfun _ k) -> k
    -- deliberately missing instance

-- * some default types
tUnit = TCon (Tycon "()" Star)
tInteger = TCon (Tycon "Integer" Star)
tFloat = TCon (Tycon "Float" Star)
tBoolean = TCon (Tycon "Boolean" Star)
tArrow   = TCon (Tycon "->" (Kfun Star (Kfun Star Star)))

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TAp (TAp tArrow a) b
