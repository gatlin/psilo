{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types
where

-- |
-- Inspiration for this code came from
--   https://brianmckenna.org/blog/type_annotation_cofree
--
-- I also liberally plagiarized this page:
--   http://dev.stephendiehl.com/fun/006_hindley_milner.html#generalization-and-instantiation
--
-- Finally, the paper "Typing Haskell in Haskell" was plagiarized once I
-- realized it's where the others were plagiarizing from.
--
-- It has been modified for a slightly different type and more flexible type
-- grammar.

import Control.Monad (forM_, forM, foldM, liftM2, zipWithM, mapAndUnzipM, when)
import Control.Monad.Free
import Data.Functor.Identity
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List (nub, union, intercalate, sort)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid
import Data.Either (either)

import Control.Comonad
import Control.Comonad.Cofree

import Data.Foldable (Foldable, fold)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Lib.Syntax
import Lib.Parser (parse_expr, parse_multi)
import Lib.Util

typeInt, typeBool, typeFloat :: Type
typeInt = TSym (TyCon "Int" Star)
typeBool = TSym (TyCon "Boolean" Star)
typeFloat = TSym (TyCon "Float" Star)

-- * Type building blocks

type Sym = String

-- | Kinds are like type-level types
data Kind
    = Star
    | Kind :-> Kind
    deriving (Eq, Ord)

instance Show Kind where
    show (Star) = "*"
    show (a :-> b) = (show a) ++ " -> " ++ (show b)

class HasKind t where
    kind :: t -> Kind

-- | A type variable is a unique identifier of some sort and a 'Kind'
data TyVar = TyVar Int Kind
    deriving (Eq, Ord)

-- | For printing out 'TyVar's
nsym :: Int -> Symbol
nsym n = l ++ suffix where
    letters = ['a' .. 'z']
    idx = n `mod` (length letters)
    l = [ letters !! idx ]
    suff 0 = ""
    suff s = show s
    suffix = suff $ n `div` (length letters)

instance Show TyVar where
    show (TyVar n k) = nsym n

instance HasKind TyVar where
    kind (TyVar _ k) = k

-- | A type constant is a unique symbol along with a 'Kind'
data TyCon = TyCon Symbol Kind
    deriving (Eq, Ord)

instance Show TyCon where
    show (TyCon sym k) = sym

instance HasKind TyCon where
    kind (TyCon _ k) = k

-- | Types are like kind-level values
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
    kind (TFun (t:ts)) = Star

-- | A predicate is essentially an assertion about a type
data Pred = IsIn Symbol Type deriving (Eq, Ord)

instance Show Pred where
    show (IsIn c t) = c ++ " " ++ (show t)

-- | A qualified value is associated with a list of predicates
data Qual t = [Pred] :=> t deriving (Eq, Ord)

instance Show t => Show (Qual t) where
    show ([] :=> t) = show t
    show (ps :=> t) = "(" ++ ps' ++ ")" ++ " => " ++ show t
        where ps' = intercalate ", " $ map show ps

-- * Frames and substitution
type Frame = Map TyVar Type

nullFrame :: Frame
nullFrame = M.empty

(|->) :: TyVar -> Type -> Frame
u |-> t = M.fromList $ [(u, t)]

compose :: Frame -> Frame -> Frame
f1 `compose` f2 = M.map (substitute f1) f2 `M.union` f1

-- | Things which have free variables and which may have 'Frame' substitutions
-- applied on them
class TypeLike t where
    substitute :: Frame -> t -> t
    ftv :: t -> Set TyVar

instance TypeLike Type where
    ftv (TVar n) = S.singleton n
    ftv (TFun ts) = foldl (<>) mempty $ fmap ftv ts
    ftv _ = mempty

    substitute frame (TVar u) = case M.lookup u frame of
        Just t -> t
        Nothing -> TVar u

    substitute frame (TFun ts) = TFun (substitute frame ts)

    substitute frame t = t

instance TypeLike a => TypeLike [a] where
    ftv = foldr (S.union . ftv) S.empty
    substitute = map . substitute

instance TypeLike Pred where
    substitute frame (IsIn i t) = IsIn i (substitute frame t)
    ftv (IsIn i t) = ftv t

instance TypeLike t => TypeLike (Qual t) where
    substitute frame (ps :=> t) =
        (substitute frame ps) :=> (substitute frame t)

    ftv (ps :=> t) = (ftv ps) `S.union` (ftv t)

-- * Let-polymorphism

-- | A polymorphic, universally quantified type for the top-level.
data Scheme = Forall [TyVar] (Qual Type)
    deriving (Eq, Ord)

-- | Substitute type variables in a 'Scheme' with variables starting at 0.
normalize :: Scheme -> Scheme
normalize (Forall _ (ps :=> t)) = Forall (map snd ord) (ps' :=> (normtype t))
    where
        ord = zip (nub $ fv t) (map (\n -> TyVar n Star) [0..])
        ps' = map (\(IsIn sym (TVar t)) -> maybe
                                    (error "non-existent type variable")
                                    (\x -> (IsIn sym (TVar x)))
                                    (Prelude.lookup t ord))
              ps
        fv = reverse . S.toList . ftv

        normtype (TFun ts) = TFun $ map normtype ts
        normtype (TSym sym) = TSym sym
        normtype (TVar tv) = case Prelude.lookup tv ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"

instance Show Scheme where
    show (Forall vars t) = prefix ++ (show t)
        where vars' = intercalate " " $ map show vars
              prefix = if (length vars) > 0 then "∀ " ++ vars' ++ ". "
                                            else ""

instance TypeLike Scheme where
    ftv (Forall vars t) = (ftv t) `S.difference` (S.fromList vars)

    substitute frame (Forall vars t) = Forall vars $
        substitute (foldr M.delete frame vars) t

-- | The type of errors we might encounter during inference and solving.
data TypeError
    = UnificationFail Type Type
    | UnificationMismatch [Type] [Type]
    | InfiniteType TyVar Type
    | UnboundVariable Sym
    deriving (Eq, Show)

-- * Type Inference and Constraint Generation

-- | A mapping from top-level symbols to type schemes.
newtype TypeEnv = TypeEnv (Map Sym Scheme) deriving (Monoid, Show)

instance TypeLike TypeEnv where
    substitute frame (TypeEnv env) = TypeEnv $ M.map (substitute frame) env
    ftv (TypeEnv env) = ftv $ M.elems env

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv M.empty

extendEnv :: TypeEnv -> (Sym, Scheme) -> TypeEnv
extendEnv (TypeEnv env) (x, s) = TypeEnv $ M.insert x s env

typeof :: TypeEnv -> Sym -> Maybe Scheme
typeof (TypeEnv env) name = M.lookup name env

envRemove :: TypeEnv -> Sym -> TypeEnv
envRemove (TypeEnv env) var = TypeEnv $ M.delete var env

envLookup :: TypeEnv -> Sym -> Maybe Scheme
envLookup (TypeEnv env) sym = M.lookup sym env

-- | Generalize a qualified type into a type scheme in a given context
generalize :: TypeEnv -> Qual Type -> Scheme
generalize te t = Forall as t
    where as = S.toList $ ftv t `S.difference` ftv te

-- | Neatly lift qualified types up into type schemes
closeOver :: Frame -> Qual Type -> Scheme
closeOver f qt = normalize $ generalize mempty (substitute f qt)

-- | The state we need to mutate during inference.
data InferState = InferState
    { varCount :: Int -- ^ Monotonically increasing type ID number.
    }

initInferState :: InferState
initInferState = InferState 0

-- | A constraint is generated during type inference and is used to solve for
-- type signatures.
data Constraint
    = Type := Type -- ^ Equality constraint: two types are identical
    | Type :~ [Pred]
    deriving (Eq, Ord, Show)

instance TypeLike Constraint where
    substitute frame c = case c of
        (t1 := t2) -> (substitute frame t1) := (substitute frame t2)
        (tv :~ ps) -> (substitute frame tv) :~ (substitute frame ps)

    ftv c = case c of
        (t1 := t2) -> (ftv t1) `S.union` (ftv t2)
        (tv :~ ps) -> (ftv tv) `S.union` (ftv ps)

-- | The @Infer@ monad:
-- 1. reads and locally extends a type environment;
-- 2. produces a log of inferred type constraints; and
-- 3. references a mutable counter to generate new type variables.
-- The result of inference is either a 'TypeError' or a 3-tuple containing
-- the computed types, the final inference state, and the generated
-- constraints.
type Infer = RWST
    TypeEnv      -- type environment
    [Constraint] -- produced constraints
    InferState   -- mutable inference state
    (Except TypeError) -- Inference errors

-- | Execute an 'Infer'ence.
runInfer
    :: TypeEnv
    -> InferState
    -> Infer a
    -> Except TypeError (a, InferState, [Constraint])
runInfer te inferState m = runRWST m te inferState

-- helper to record an equality constraint during inference
(@=) :: Type -> Type -> Infer ()
t1 @= t2 = tell [t1 := t2]

(@~) :: Type -> [Pred] -> Infer ()
ty @~ [] = return ()
ty @~ ps = tell [ty :~ ps]

-- temporarily extend the type environment for lambda abstraction
withEnv :: [(Sym, Scheme)] -> Infer a -> Infer a
withEnv xs m = do
    let scope e = foldl
                  (\e' (sym, scheme) ->
                       (envRemove e' sym) `extendEnv` (sym, scheme))
                  e
                  xs
    local scope m

getEnv :: Infer TypeEnv
getEnv = ask

-- | Generate a fresh type variable with a specific 'Kind'
fresh :: Kind -> Infer TyVar
fresh k = do
    c <- gets varCount
    modify $ \st -> st { varCount = c + 1 }
    return $ TyVar c k

-- | Instantiate a type scheme into a qualified type (opposite of 'generalize')
instantiate :: Scheme -> Infer (Qual Type)
instantiate (Forall vs t) = do
    vs' <- mapM (const $ fresh Star) vs >>= mapM (return . TVar)
    let frame = M.fromList $ zip vs vs'
    return $ substitute frame t

-- | Constraint generation and type generation
infer :: AnnotatedExpr a -> Infer Type

-- Constants are straightforward, except integers could be any @Num@ instance.
infer (_ :< IntC _) = do
    ty <- fresh Star >>= \tv -> return $ TVar tv
    ty @~ [IsIn "Num" ty]
    return ty
infer (_ :< BoolC _) = return $ typeBool
infer (_ :< FloatC _) = return $ typeFloat

-- Symbols are either not in the type environment, raising an exception, or they
-- map to a 'Scheme' which may be instantiated. In this way we allow
-- let-polymorphism for top-level definitions.
infer (_ :< IdC sym) = do
    tEnv <- getEnv
    var <- fresh Star >>= return . TVar
    case envLookup tEnv sym of
        Nothing -> throwError $ UnboundVariable sym
        Just scheme -> do
            qt@(ps :=> ty) <- instantiate scheme
            var @= ty
            ty @~ ps
            return var

-- | A lambda abstraction is a list of symbols and an 'AnnotatedExpr' body. Each
-- argument should generate a unique 'Scheme' and the 'TypeEnv' should be
-- temporarily extended with them to evaluate the body.
infer (_ :< FunC args body) = do
    argVars <- forM args $ const (fresh Star >>= return . TVar)
    argScms <- forM argVars $ \v -> do
        return $ Forall [] ([] :=> v)
    br <- withEnv (zip args argScms) $ infer body
    let fun_ty = TFun $ argVars ++ [br]
    return fun_ty

-- | Lambda application is straightforward: infer types for the operator and the
-- operands, generate a fresh type variable for the return value, and assert
-- that the operator is equivalent to a function consuming the operands and
-- producing the return value.
infer (_ :< AppC op erands) = do
    op' <- infer op
    erands' <- mapM infer erands
    var <- fresh Star >>= return . TVar
    op' @= (TFun $ erands' ++ [var])
    return var

-- | Assert that the condition expression is a boolean and that the two branches
-- are the same thing.
infer (_ :< IfC c t e) = do
    cr <- infer c
    tr <- infer t
    er <- infer e
    cr @= typeBool
    tr @= er
    return tr

-- * Constraint solving

-- | A map from type variables to predicates
newtype PredMap = PMap (Map Type [Pred])
    deriving (Monoid, Show)

instance TypeLike PredMap where
    ftv (PMap m) = ftv (M.elems m)
    substitute frame (PMap m) = PMap $
        M.mapKeys (substitute frame) $
        M.map (substitute frame) m

updatePredMap :: Type -> Pred -> PredMap -> PredMap
updatePredMap ty p (PMap m) = PMap $ case M.lookup ty m of
    Nothing -> M.insert ty [p] m
    Just ps -> M.insert ty (p:ps) m

lookupPreds :: Type -> PredMap -> [Pred]
lookupPreds ty (PMap m) = concat $ fmap go tvs where
    tvs = S.toList $ ftv ty
    go tv = case M.lookup (TVar tv) m of
        Nothing -> []
        Just ps -> ps

-- | A frame paired with constraints to solve.
type Unifier = (Frame, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (mempty, [])

-- | A monad for solving constraints. The state is a 'Unifier' being
-- constructed. Execution may result in a raised 'TypeError'.
type Solve = StateT (Frame, [Constraint], PredMap) (Except TypeError)

-- | Unification of two 'Type's.
unify :: Type -> Type -> Solve Unifier
unify t1 t2 | t1 == t2 = return emptyUnifier
unify (TVar v) t = v `bind` t
unify t (TVar v) = v `bind` t
unify (TFun as) (TFun bs) = unifyMany as bs
unify t1 t2 = throwError $ UnificationFail t1 t2

-- | Unification of a list of 'Type's.
unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
    (su1, cs1) <- unify t1 t2
    (su2, cs2) <- unifyMany (substitute su1 ts1) (substitute su1 ts2)
    return (su2 `compose` su1, nub $ cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Bind a 'TyVar' to a 'Type' in the 'Frame', unless the result would be an
-- infinite type.
bind :: TyVar -> Type -> Solve Unifier
bind a t | t == TVar a = return emptyUnifier
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = do
               return (M.singleton a t, [])

-- | Ensure that a 'TyVar' is not free in a 'Type'.
occursCheck :: TypeLike a => TyVar -> a -> Bool
occursCheck a t = a `S.member` (ftv t)

-- | The actual solving algorithm. Reads the current 'Unifier' and iterates
-- through the constraints generating 'Unifier's. These are then merged into the
-- state.
solver :: Solve (Frame, PredMap)
solver = do
    (su, cs, pm) <- get
    case cs of
        [] -> return (su, pm)
        (c:cs0) -> case c of
            (t1 := t2) -> do
                (su1, cs1) <- unify t1 t2
                put (su1 `compose` su, nub (cs1 ++ (substitute su1 cs0)), pm)
                solver
            (ty :~ ps) -> do
                let pm' = foldl
                          (\pm pred@(IsIn _ ty) ->
                               updatePredMap ty pred pm)
                          pm ps
                put (su, cs0, pm')
                solver

runSolve :: [Constraint] -> Except TypeError (Frame, PredMap)
runSolve cs = evalStateT solver (mempty, cs, mempty)

-- * Defaults!

num_binop :: Qual Type
num_binop = [IsIn "Num" t_0] :=> (TFun [t_0, t_0, t_0])
    where t_0 = TVar (TyVar 0 Star)

eq_binop :: Qual Type
eq_binop = [IsIn "Eq" t_0] :=> (TFun [t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

ord_binop :: Qual Type
ord_binop = [IsIn "Ord" t_0] :=> (TFun [t_0, t_0, typeBool])
    where t_0 = TVar (TyVar 0 Star)

-- | The default type environment.
defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv $ M.fromList
    [ ("*", generalize mempty num_binop)
    , ("+", generalize mempty num_binop)
    , ("-", generalize mempty num_binop)
    , ("/", generalize mempty num_binop)
    , ("=", generalize mempty eq_binop)
    , ("<", generalize mempty ord_binop)
    , (">", generalize mempty ord_binop)
    , ("id", generalize mempty $ [] :=> (TFun [TVar (TyVar 0 Star),
                                               TVar (TyVar 0 Star)]))
    ]

-- | Right now this is a two-pass inferencer.
-- The first pass infers and solves for generic type schemes for each
-- definition. These are then inserted into the default type environment and
-- inference and solving are re-run to produce the final type schemes.
-- TODO: handle predicates.
typecheck_defns
    :: [Definition]
    -> TypeEnv
    -> Either TypeError ([Scheme], [Constraint])
typecheck_defns defns te = runExcept $ do
    let te' = defaultTypeEnv <> te
    (syms, exprs) <- mapAndUnzipM
                     (\(Define sym expr) -> return (sym, annotated expr))
                     defns
    (tys_pass_1, _, cs1) <- runInfer te' initInferState $ do
        scms <- forM exprs $ \_ -> do
            tv <- fresh Star
            return $ generalize te' $ [] :=> (TVar tv)
        inferred <- withEnv (zip syms scms) $ mapM infer exprs
        return inferred

    (frame1, pm1) <- runSolve cs1
    let scms1 = map (\ty -> closeOver frame1 ([] :=> ty)) tys_pass_1
    let te = foldl extendEnv te' $ zip syms scms1
    (tys_pass_2, _, cs2) <- runInfer te initInferState $ mapM infer exprs
    (frame2, pm2) <- runSolve cs2
    let pm = substitute frame2 pm2
    let scms2 = fmap (\ty ->
                         closeOver frame2
                         ((lookupPreds ty pm) :=> (substitute frame2 ty)))
                tys_pass_2
    return (scms2, substitute frame2 cs2)


typecheck_defn
    :: Definition
    -> TypeEnv
    -> Either TypeError (Scheme, [Constraint])
typecheck_defn defn te = case typecheck_defns [defn] te of
    Left err -> Left err
    Right ((ty:_), cs) -> Right (ty, cs)

-- * Test shit
example_defns = [ "(def three (id 3.0))"
                , "(defun times-2 (x) (* x 2.0))"
                , "(def eight (times-2 4.0))"
                , "(defun square (x) (* x x))"
                , "(def nine (square 3.0))"
                , "(def four (square 2))"
                , "(defun fact (n) (if (< n 2) n (fact (* n (- n 1)))))"
                , "(defun compose (f g x) (f (g x)))"
                , "(defun wut (x) (if (< x 2) x (- x 1)))"
                ]

test :: IO ()
test = do
    mDefns <- parse_multi . T.pack . concat $ example_defns
    case mDefns of
        Nothing -> putStrLn "Parser error T_T"
        Just defns -> do
            let defns' = fmap (fromJust . surfaceToDefinition) defns
            case typecheck_defns defns' defaultTypeEnv of
                Left err -> putStrLn . show $ err
                Right (schemes, cs) -> do
                    putStrLn "Schemes"
                    putStrLn "---"
                    forM_ (zip example_defns schemes) $ \(d, s) -> do
                        putStrLn $ d ++ " : " ++ (show s)
                    putStrLn "Constraints"
                    putStrLn "---"
                    forM_ cs $ putStrLn . show
