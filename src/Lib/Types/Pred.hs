module Lib.Types.Pred
where

import Data.List (union, (\\))
import Control.Monad (msum)

import Lib.Syntax (Symbol)
import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Subst
import Lib.Types.Unify

data Pred = IsIn Symbol [Type] deriving (Eq)

predHead :: Pred -> Symbol
predHead (IsIn i ts) = i

instance Show Pred where
    show (IsIn i [t]) = i ++ " " ++ show t
    show (IsIn i ts) = i ++ " " ++ show ts

data Qual t = [Pred] :=> t
    deriving (Eq)

instance Show t => Show (Qual t) where
    show (ps :=> t) = show ps ++ " => " ++ show t

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tv (ps :=> t) = tv ps `union` tv t

instance Types Pred where
    apply s (IsIn i ts) = IsIn i (apply s ts)
    tv (IsIn i ts) = tv ts

instance Unify Pred where
    mgu = liftPred mgu

instance Match Pred where
    match = liftPred match

liftPred m (IsIn i ts) (IsIn i' ts')
    | i == i' = m ts ts'
    | otherwise = fail "classes differ"

type Class = ([Tyvar], [Pred], [Inst])
type Inst = Qual Pred

-----

data ClassEnv = ClassEnv
    { classes :: Symbol -> Maybe Class
    , defaults :: [Type]
    }

sig :: ClassEnv -> Symbol -> [Tyvar]
sig ce i = case classes ce i of Just (vs, is, its) -> vs

super :: ClassEnv -> Symbol -> [Pred]
super ce i = case classes ce i of Just (vs, is, its) -> is

insts :: ClassEnv -> Symbol -> [Inst]
insts ce i = case classes ce i of Just (vs, is, its) -> its

defined :: Maybe a -> Bool
defined (Just x) = True
defined (Nothing) = False

modify :: ClassEnv -> Symbol -> Class -> ClassEnv
modify ce i c = ce {
    classes = \j -> if i == j then Just c else classes ce j }

initialEnv :: ClassEnv
initialEnv = ClassEnv {
    classes = \i -> fail "class not defined",
    defaults = [tInteger, tFloat]
    }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do
    ce' <- f ce
    g ce'

addClass :: Symbol -> [Tyvar] -> [Pred] -> EnvTransformer
addClass i vs ps ce
    | defined (classes ce i) = fail "class already defined"
    | any (not . defined . classes ce . predHead) ps = fail "superclass not defined"
    | otherwise = return (modify ce i (vs, ps, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

atyvar = Tyvar "a" Star
atype = TVar atyvar
asig = [atyvar]

mtyvar = Tyvar "m" (Kfun Star Star)
mtype = TVar mtyvar
msig = [mtyvar]

addCoreClasses :: EnvTransformer
addCoreClasses =   addClass "Eq" asig []
               <:> addClass "Ord" asig [IsIn "Eq" [atype]]
               <:> addClass "Show" asig []
               <:> addClass "Enum" asig []
               <:> addClass "Functor" msig []

addNumClasses :: EnvTransformer
addNumClasses =   addClass "Num" asig [IsIn "Eq" [atype],
                                       IsIn "Show" [atype]]
              <:> addClass "Real" asig [IsIn "Num" [atype],
                                        IsIn "Ord" [atype]]
              <:> addClass "Fractional" asig [IsIn "Num" [atype]]
              <:> addClass "Integral" asig [IsIn "Real" [atype],
                                            IsIn "Enum" [atype]]

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
    | not (defined (classes ce i)) = fail "no class for instance"
    | any (overlap p) qs = fail "overlapping instance"
    | otherwise = return (modify ce i c)
    where its = insts ce i
          qs = [ q | (_ :=> q) <- its ]
          c = (sig ce i, super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined (mgu p q)

exampleInsts :: EnvTransformer
exampleInsts =   addPreludeClasses
             <:> addInst [] (IsIn "Ord" [tUnit])
             <:> addInst [] (IsIn "Ord" [tInteger])

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i ts) = p : concat (map (bySuper ce) supers)
    where supers = apply s (super ce i)
          s = zip (sig ce i) ts

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [ tryInst it | it <- insts ce i ]
    where tryInst (ps :=> h) = do
              u <- match h p
              Just (map (apply u) ps)

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case byInst ce p of
                     Nothing -> False
                     Just qs -> all (entail ce ps) qs

simplify :: ([Pred] -> Pred -> Bool) -> [Pred] -> [Pred]
simplify ent = loop []
    where loop rs [] = rs
          loop rs (p:ps)
              | ent (rs ++ ps) p = loop rs ps
              | otherwise = loop (p:rs) ps

reduce :: ClassEnv -> [Pred] -> [Pred]
reduce ce = simplify (scEntail ce) . elimTauts ce

elimTauts :: ClassEnv -> [Pred] -> [Pred]
elimTauts ce ps = [ p | p <- ps, not (entail ce [] p) ]

scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)
