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
data TyCon = TyCon Symbol Kind deriving (Eq, Ord)

instance Show TyCon where
    show (TyCon sym k) = sym

instance HasKind TyCon where
    kind (TyCon _ k) = k

-- | Types are like kind-level values :)
data Type
    = TVar TyVar
    | TSym TyCon
    | TFun [Type]
    | TForall [TyVar] Type -- Sigma types
    | [Pred] :=> Type -- Qualified types
    | IsIn Symbol Type -- Predicates
    deriving (Ord, Eq)

-- | Type alias for refactor convenience because initially predicates were a
-- separate type
type Pred = Type

parensShow :: Type -> String
parensShow ty@(TFun ts) = "(" ++ (show ty) ++ ")"
parensShow ty           = show ty

instance Show Type where
    show (TForall vs t) = prefix ++ (show t)
        where vs' = intercalate " " $ map show vs
              prefix = "∀" ++ vs' ++ ". "
    show (TVar n) = show n
    show t@(TSym sym) = show sym
    show t@(TFun ts) = go ts where
        go [] = ""
        go ((TSym (TyCon "->" _)):t:[]) = "-> " ++ (show t)
        go ((TSym (TyCon "->" Star)):ts') = intercalate " -> " $
            map parensShow ts'
        go ts' = intercalate " " $ map parensShow ts'
    show (IsIn c t) = c ++ " " ++ (show t)
    show ([] :=> t) = show t
    show (ps :=> t) = "(" ++ ps' ++ ")" ++ " => " ++ show t
        where ps' = intercalate ", " $ map show ps

instance HasKind Type where
    kind (TForall vs t)                    = kind t
    kind (TSym tc)                         = kind tc
    kind (TVar tv)                         = kind tv
    kind (TFun (t:ts)) = go ts where

        go ts | length ts == 1 = (Star :-> Star) -- nullary functions
              | otherwise = go' ts

        go' (t:[]) = kind t
        go' (t:ts) = (kind t) :-> (go' ts)

    kind (ps :=> t)                        = kind t
    kind (IsIn c t)                        = kind t

typeInt, typeBool, typeFloat :: Type
typeInt = TSym (TyCon "Int" Star)
typeBool = TSym (TyCon "Boolean" Star)
typeFloat = TSym (TyCon "Float" Star)

tyFun :: Type
tyFun = TSym $ TyCon "->" Star
