module Lib.Types.Type where

import           Data.List         (intercalate)
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Set          (Set)
import qualified Data.Set          as S
import           Lib.Syntax.Symbol
import           Lib.Types.Kind

-- | A type variable is a unique identifier of some sort and a 'Kind'
data TyVar = TyVar Int Kind deriving (Eq, Ord)

-- | For printing out 'TyVar's
nsym :: Int -> Symbol
nsym n = l ++ suffix where
    letters = ['a' .. 'z']
    idx = n `mod` (length letters)
    l = [letters !! idx]
    suff 0 = ""
    suff s = show s
    suffix = suff $ n `div` (length letters)

instance Show TyVar where
    show (TyVar n k) = nsym n

instance HasKind TyVar where
    kind (TyVar _ k) = k

-- | A type constant is a unique symbol along with a 'Kind'
data TyLit = TyLit Symbol Kind deriving (Eq, Ord)

instance Show TyLit where
    show (TyLit sym k) = sym

instance HasKind TyLit where
    kind (TyLit _ k) = k

-- | Types are like kind-level values :)
-- The theoretical type grammar is based on [1], with the addition of qualified
-- types.
--
--   Type variables a, b
--   Type constants K
--   Qualified type : qual ::= [<symbol> <tau>...] => <rho>
--   Sigma types :    sigma ::= forall [a...] <rho>
--   Rho types :      rho   ::= <tau> | <sigma> -> <sigma> | <qual>
--   Tau types :      tau   ::= K | <tau> -> <tau> | a
--
-- There are a number of invariants that are (should be) maintained throughout
-- the code, but the grammar itself permits all sorts of violations. Perhaps in
-- an optimistic future we can encode more safety into the grammar.
--
-- [1]: Practical Type inference for arbitrary-rank types,
--      Peyton-Jones et al
data Type
    = TVar TyVar
    | TSym TyLit
    | TList [Type]
    | TForall [TyVar] Type -- Sigma types
    | [Pred] :=> Type -- Qualified types
    | IsIn Symbol [Type] -- Predicates
    deriving (Ord, Eq)

-- | Type aliases for clarity throughout the project
type Sigma = Type
type Rho = Type
type Tau = Type

-- | Type alias for refactor convenience because initially predicates were a
-- separate type
type Pred = Type

parensShow :: Type -> String
parensShow ty@(TList ts) = "(" ++ (show ty) ++ ")"
parensShow ty            = showType ty


showType (TForall vs t) = "(" ++ prefix ++ (showType t) ++ ")"
    where vs' = intercalate " " $ map show vs
          prefix = "∀" ++ vs' ++ ". "
showType (TVar n) = show n
showType t@(TSym sym) = show sym
showType t@(TList ts) = go ts where
    go [] = "()"
    go ((TSym (TyLit "->" _)):t:[]) = "-> " ++ (showType t)
    go ((TSym (TyLit "->" Star)):ts') = intercalate " -> " $
        map parensShow ts'
    go ts' = intercalate " " $ map parensShow ts'
showType (IsIn c t) = c ++ " " ++ (show t)
showType (ps :=> t) = "[" ++ ps' ++ "]" ++ " => " ++ showType t
    where ps' = intercalate ", " $ map showType ps

instance Show Type where
    show = showType

instance HasKind Type where
    kind (TForall vs t) = kind t
    kind (TSym tc)      = kind tc
    kind (TVar tv)      = kind tv
    kind (TList (t:_))  = kind t


    kind (ps :=> t)     = kind t
    kind (IsIn c t)     = kind (t !! 0) -- HACK BAD FIXME TODO

typeInt, typeBool, typeFloat :: Type
typeInt = TSym (TyLit "Int" Star)
typeBool = TSym (TyLit "Boolean" Star)
typeFloat = TSym (TyLit "Float" Star)

tyFun :: Type
tyFun = TSym $ TyLit "->" Star

removeEmptyPreds :: Type -> Type
removeEmptyPreds ([] :=> t)     = removeEmptyPreds t
removeEmptyPreds (ps :=> t)     = ps :=> (removeEmptyPreds t)
removeEmptyPreds (TForall vs t) = TForall vs (removeEmptyPreds t)
removeEmptyPreds (TList tys)    = TList $ fmap removeEmptyPreds tys
removeEmptyPreds t              = t
