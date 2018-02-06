module Lib.Types.Type where

import Lib.Syntax.Symbol
import Lib.Types.Kind
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

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
    deriving (Ord, Eq)

instance Show Type where
    show (TVar n) = show n
    show (TSym sym) = show sym
    show (TFun ts) = parens ts' where
        parens inside = "(" ++ inside ++ ")"
        ts' = intercalate " -> " $ map show ts

instance HasKind Type where
    kind (TSym tc) = kind tc
    kind (TVar tv) = kind tv
    kind (TFun (t:ts)) = Star -- FIXME NOT ALWAYS

typeInt, typeBool, typeFloat :: Type
typeInt = TSym (TyCon "Int" Star)
typeBool = TSym (TyCon "Boolean" Star)
typeFloat = TSym (TyCon "Float" Star)
