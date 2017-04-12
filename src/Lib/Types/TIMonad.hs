module Lib.Types.TIMonad
where

import Lib.Syntax (Symbol)
import Lib.Types.Kind
import Lib.Types.Type
import Lib.Types.Pred
import Lib.Types.Subst
import Lib.Types.Unify
import Lib.Types.Scheme

import Control.Applicative
import Control.Monad (ap, liftM)

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
                                 (s', m, x) -> let TI gx = g x
                                               in  gx s' m)

-- I'm stealing code from an older library written before the
-- Monad-Applicative-Functor proposal was adopted.
instance Applicative TI where
    pure = return
    (<*>) = ap

instance Functor TI where
    fmap = liftM

runTI :: TI a -> a
runTI (TI f) = x where (s, n, x) = f nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Type -> Type -> TI ()
unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extSubst u

trim :: [Tyvar] -> TI ()
trim vs = TI $ \s n ->
                   let s' = [ (v,t) | (v,t) <- s, v `elem` vs ]
                       force = length (tv (map snd s'))
                   in  force `seq` (s', n, ())

extSubst :: Subst -> TI ()
extSubst s' = TI $ \s n -> (s' @@ s, n, ())

newTVar :: Kind -> TI Type
newTVar k = TI $ \s n ->
                     let v = Tyvar (enumId n) k
                     in  (s, n+1, TVar v)

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
    ts <- mapM newTVar ks
    return (inst ts qt)

enumId :: Int -> Symbol
enumId n = "v" ++ show n

class Instantiate t where
    inst :: [Type] -> t -> t

instance Instantiate Type where
    inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
    inst ts (TGen n) = ts !! n
    inst ts t = t

instance Instantiate a => Instantiate [a] where
    inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
    inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)
