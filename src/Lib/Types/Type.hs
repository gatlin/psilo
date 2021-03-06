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
--   Sigma types :    sigma ::= forall [a...] <rho>
--   Rho types :      rho   ::= <tau> | <sigma> -> <sigma> | <qual>
--   Tau types :      tau   ::= K | <tau> -> <tau> | a
--   Qualified type : qual  ::= [<symbol> <tau>...] => <tau>
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
    | TPred Symbol [Type]
    | [Pred] :=> Type
    deriving (Ord, Eq)

-- | Type aliases for clarity throughout the project.
type Sigma = Type
type Rho = Type
type Tau = Type
type Pred = Type
type Qual = Type

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
showType (TPred sym tys) = "{ " ++ sym ++ " " ++ (show tys) ++ " }"
showType (ps :=> ty) = (show ps) ++ " => " ++ (show ty)

instance Show Type where
    show = showType

instance HasKind Type where
    kind (TForall vs t) = kind t
    kind (TSym tc)      = kind tc
    kind (TVar tv)      = kind tv
    kind (TList (t:_))  = kind t
    kind (TPred _ tys)  = kind $ tys !! 0

typeInt, typeBool, typeFloat :: Type
typeInt = TSym (TyLit "Int" Star)
typeBool = TSym (TyLit "Boolean" Star)
typeFloat = TSym (TyLit "Float" Star)
typeByte = TSym (TyLit "Byte" Star)

tyFun :: Type
tyFun = TSym $ TyLit "->" Star

removeEmptyPreds :: Type -> Type
removeEmptyPreds ([] :=> t)     = removeEmptyPreds t
removeEmptyPreds (TForall vs t) = TForall vs (removeEmptyPreds t)
removeEmptyPreds (TList tys)    = TList $ fmap removeEmptyPreds tys
removeEmptyPreds t              = t

alias_rewrite :: Symbol -> [TyVar] -> Type -> Sigma -> Sigma
alias_rewrite sym vars ty sig = case sig of
    TSym (TyLit lit _) -> if lit == sym then ty else sig
    TList [] -> TList []
    (TList (t@(TSym (TyLit lit _)) : tys)) ->
        if lit == sym
        then ty
        else TList $ fmap (alias_rewrite sym vars ty) (t:tys)
    TForall vs t -> TForall vs $ alias_rewrite sym vars ty t
    ps :=> t ->
        (fmap (alias_rewrite sym vars ty) ps) :=> (alias_rewrite sym vars ty t)
    TPred s t -> TPred s $ fmap (alias_rewrite sym vars ty) t
    t -> t
