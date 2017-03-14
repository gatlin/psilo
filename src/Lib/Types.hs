{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Control.Monad.State hiding (sequence)
import Control.Monad.IO.Class
import Data.List (nub, union, intercalate, sort)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Data.Monoid

import Control.Comonad
import Control.Comonad.Cofree
import Data.Foldable (Foldable, fold)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.Set (Set)
import qualified Data.Set as S

import Lib.Syntax
import Lib.Parser (parse_expr, parse_multi)
import qualified Data.Text as T

-- * Fundamental type system building blocks

-- | Kinds are like type level type signatures.
data Kind
    = Star
    | Kind :-> Kind
    deriving (Eq, Ord)

instance Show Kind where
    show (Star) = "*"
    show (a :-> b) = (show a) ++ " -> " ++ (show b)

-- | Things that have 'Kind' values.
class HasKind t where
    kind :: t -> Kind

-- | A type variable will have some distinguishing ID along with 'Kind'.
data TyVar = TyVar Int Kind
    deriving (Eq, Ord)

instance Show TyVar where
    show (TyVar n k) = "t"++(show n)

instance HasKind TyVar where
    kind (TyVar n k) = k

-- | A type constant is a unique symbol along with an associated 'Kind'.
data TyCon = TyCon Symbol Kind
    deriving (Eq, Ord)

instance Show TyCon where
    show (TyCon sym k) = sym

instance HasKind TyCon where
    kind (TyCon _ k) = k

-- | The type language. Perhaps this will change but for flexibility types are
-- composed of variables, constant symbols, or function arrows.
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
        ts' = intercalate " -> " $
              map show ts

instance HasKind Type where
    kind (TSym tc) = kind tc
    kind (TVar tv) = kind tv
    kind (TFun (t:ts)) = Star -- not that important right now but FIXME anyway

-- | This function mainly exists for readability.
(|->) :: [ Type ] -> Type -> Type
args |-> body = TFun $ args ++ [body]
infix |->


-- | A mapping from type variables to concrete types. The goal of type inference
-- is to construct a frame for a given program that is consistent and sound.
newtype Frame = Frame {
    bindings :: Map TyVar Type
    } deriving (Eq, Show)

instance Monoid Frame where
    mempty = Frame mempty
    (Frame f1) `mappend` (Frame f2) = Frame $
        M.map (substitute (Frame f1)) f2 `M.union` f1

frameLookup :: Frame -> TyVar -> Maybe Type
frameLookup (Frame frame) i = M.lookup i frame

frameInsert :: Frame -> TyVar -> Type -> Frame
frameInsert (Frame frame) i ty = Frame $ M.insert i ty frame

frameDelete :: Frame -> TyVar -> Frame
frameDelete (Frame frame) i = Frame $ M.delete i frame

-- | A class for things which may contain type variables or be substituted for
-- types given a 'Frame'.
class Typing t where
    -- | free type variables
    ftv :: t -> Set TyVar

    -- | perform a substitution
    substitute :: Frame -> t -> t

instance Typing Type where
    ftv (TVar n) = S.singleton n
    ftv (TSym s) = mempty
    ftv (TFun ts) = foldl (<>) mempty $ fmap ftv ts

    substitute frame v@(TVar n) = maybe v (substitute frame) $
        frameLookup frame n

    substitute frame (TFun ts) = TFun $ map (substitute frame) ts
    substitute _ t = t

instance Typing a => Typing [a] where
    ftv = foldr (S.union . ftv) S.empty
    substitute = map . substitute

-- ** Typeclass machinery

data Qual t = [Pred] :=> t
    deriving (Eq, Ord)

instance Show t => Show (Qual t) where
    show ([] :=> t) = show t
    show (ps :=> t) = "(" ++ ps' ++ ")" ++ " => " ++ show t
        where ps' = intercalate ", " $ map show ps

instance HasKind t => HasKind (Qual t) where
    kind (_ :=> t) = kind t

data Pred = IsIn Symbol Type
    deriving (Eq, Ord)

instance Show Pred where
    show (IsIn c t) = c ++ " " ++ (show t)

instance Typing Pred where
    substitute frame (IsIn i t) = IsIn i (substitute frame t)
    ftv (IsIn i t) = ftv t

instance Typing t => Typing (Qual t) where
    substitute frame (ps :=> t) = substitute frame ps :=> substitute frame t
    ftv (ps :=> t) = ftv ps `S.union` ftv t

liftUnifier
    :: (Type -> Type -> Maybe Frame -> Maybe Frame)
    -> Pred
    -> Pred
    -> Maybe Frame
    -> Maybe Frame
liftUnifier m (IsIn i t) (IsIn i' t') frame
    | i == i' = m t t' frame
    | otherwise = fail "Classes differ"

unify_pred :: Pred -> Pred -> Maybe Frame -> Maybe Frame
unify_pred = liftUnifier unify

match_pred :: Pred -> Pred -> Maybe Frame -> Maybe Frame
match_pred = liftUnifier match

type Class = ([Symbol], [Inst])
type Inst = Qual Pred

data ClassEnv = ClassEnv
    { classes :: Symbol -> Maybe Class
    , defaults :: [Type]
    }

super :: ClassEnv -> Symbol -> [Symbol]
super ce i = case classes ce i of
    Just (is, its) -> is
    Nothing -> []

insts :: ClassEnv -> Symbol -> [Inst]
insts ce i = case classes ce i of
    Just (is, its) -> its
    Nothing -> []

defined :: Maybe a -> Bool
defined = isJust

modifyClassEnv :: ClassEnv -> Symbol -> Class -> ClassEnv
modifyClassEnv ce i c = ce {
    classes = \j -> if i == j then Just c else classes ce j }

initialClassEnv :: ClassEnv
initialClassEnv = ClassEnv {
    classes = \i -> fail "class not defined",
    defaults = [int_t, float_t] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'
infixr 5 <:>

addClass :: Symbol -> [Symbol] -> EnvTransformer
addClass i is ce
    | defined (classes ce i) = fail "class already defined"
    | any (not . defined . classes ce) is = fail "superclass not defined"
    | otherwise = return (modifyClassEnv ce i (is, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

addCoreClasses :: EnvTransformer
addCoreClasses = addClass "Eq" []
             <:> addClass "Ord" ["Eq"]
             <:> addClass "Show" []
             <:> addClass "Enum" []

addNumClasses :: EnvTransformer
addNumClasses = addClass "Num" ["Eq", "Show"]
            <:> addClass "Real" ["Num", "Ord"]
            <:> addClass "Fractional" ["Num"]
            <:> addClass "Integral" ["Real", "Enum"]
            <:> addClass "Floating" ["Fractional"]

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
    | not (defined (classes ce i)) = fail "no class for instance"
    | any (overlap p) qs = fail "overlapping instance"
    | otherwise = return (modifyClassEnv ce i c)
    where its = insts ce i
          qs = [q | (_ :=> q) <- its ]
          c = (super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined (unify_pred p q $ Just mempty)

exampleInsts :: EnvTransformer
exampleInsts = addPreludeClasses
           <:> addInst [] (IsIn "Show" int_t)
           <:> addInst [] (IsIn "Eq" int_t)
           <:> addInst [] (IsIn "Ord" int_t)
           <:> addInst [] (IsIn "Num" int_t)
           <:> addInst [] (IsIn "Show" float_t)
           <:> addInst [] (IsIn "Eq" float_t)
           <:> addInst [] (IsIn "Ord" float_t)
           <:> addInst [] (IsIn "Num" float_t)
           <:> addInst [] (IsIn "Show" bool_t)
           <:> addInst [] (IsIn "Eq" bool_t)

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) =
    p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [tryInst it | it <- insts ce i]
    where tryInst (ps :=> h) = do
              u <- match_pred h p (Just mempty)
              return $ map (substitute u) ps

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case byInst ce p of
                     Nothing -> False
                     Just qs -> all (entail ce ps) qs

inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
    where hnf (TVar v) = True
          hnf (TSym s) = False
          hnf (TFun ts) = all hnf $ take ((length ts)-1) ts

toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do
    pss <- mapM (toHnf ce) ps
    return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p
    | inHnf p = return [p]
    | otherwise = case byInst ce p of
                      Nothing -> fail "context reduction"
                      Just ps -> toHnfs ce ps

simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
    where loop rs [] = rs
          loop rs (p:ps) | entail ce (rs ++ ps) p = loop rs ps
                         | otherwise = loop (p : rs) ps

reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do
    qs <- toHnfs ce ps
    return $ simplify ce qs


-- | A type scheme is a polymorphic type with quantified type variables. They
-- allow polymorphic types to instantiated in different ways depending on
-- context.
data Scheme = Forall [TyVar] (Qual Type)
    deriving (Eq, Ord)

instance Show Scheme where
    show (Forall vars t) = prefix ++ (show t)
        where vars' = intercalate " " $ map show vars
              prefix = if (length vars) > 0 then "∀ " ++ vars' ++ ". "
                                            else ""

instance HasKind Scheme where
    kind (Forall _ t) = kind t -- TODO ensure this is correct

instance Typing Scheme where
    ftv (Forall vars t) = (ftv t) `S.difference` (S.fromList vars)

    substitute (Frame frame) (Forall vars t) = Forall vars $
        substitute (Frame $ foldr M.delete frame vars) t

-- * Constraint generation

-- | If two types are constrained then there must be a valid unification for
-- them. Inference fails unless all constraints are satisfied.
data Constraint
    = Type := Type -- ^ equality constraint
    | Type :~ Scheme -- ^ instance constraint
    deriving (Show, Eq, Ord)

-- | At each step of inference we will have generated constraints along with
-- assumptions about symbols we have seen. The assumptions are used when typing
-- functions.
data TypeResult = TypeResult
    { constraints :: [ Constraint ]
    , assumptions :: Map String [ Type ]
    } deriving (Show)

instance Monoid TypeResult where
    mempty = TypeResult [] mempty
    mappend a b = TypeResult {
        constraints = constraints a <> constraints b,
        assumptions = assumptions a <> assumptions b
        }

-- | An untyped representation of an expression.
type Untyped = Cofree CoreAst ()

-- Convert a 'CoreExpr' to an 'Untyped' expression.
untyped :: Functor f => Free f t -> Cofree f ()
untyped (Free f) = () :< fmap untyped f

-- | Top-level definitions are parametrically polymorphic.
newtype TypeEnv = TypeEnv (Map Symbol Scheme)
    deriving (Show)

envInsert :: TypeEnv -> Symbol -> Scheme -> TypeEnv
envInsert (TypeEnv te) name scheme = TypeEnv $ M.insert name scheme te

instance Monoid TypeEnv where
    mempty = TypeEnv mempty
    mappend (TypeEnv a) (TypeEnv b) = TypeEnv $ a `M.union` b

instance Typing TypeEnv where
    ftv (TypeEnv te) = ftv $ M.elems te

    substitute s (TypeEnv te) = TypeEnv $ M.map (substitute s) te

-- | During inference we will want: a source of fresh, unique type variables; a
-- map to memoize inference and avoid recomputing the same expressions multiple
-- times; and a type environment for top level definitions that need to be
-- instantiated.
data TypeState = TypeState
    { varId :: Int
    , memo :: Map Untyped (Type, TypeResult)
    , typeEnv :: TypeEnv
    , definitions :: Map Symbol Untyped
    }

newTypeState :: TypeState
newTypeState = TypeState 0 mempty mempty mempty

-- | A simple type checking monad. It doesn't need to be based on IO but is for
-- lazy debugging purposes right now.
type TypeCheck = StateT TypeState IO

-- | You get a globally unique type variable each time you call this function.
freshVarId :: TypeCheck Type
freshVarId = do
    v <- gets varId
    modify $ \s -> s { varId = succ v }
    return $ TVar (TyVar v Star)

-- | The actual memoization function for the 'TypeCheck' monad.
memoizedTC
    :: (Untyped -> TypeCheck (Type, TypeResult))
    -> Untyped
    -> TypeCheck (Type, TypeResult)
memoizedTC f c@(() :< c') = gets memo >>= maybe memoize return . M.lookup c where
    memoize = case c' of
        IdC _ -> f c
        _ -> do
            r <- f c
            modify $ \s -> s { memo = M.insert c r $ memo s }
            return r

-- | Some basic helper types, which should probably be treated a little more
-- formally.
int_t = TSym (TyCon "Int" Star)
float_t = TSym (TyCon "Float" Star)
bool_t = TSym (TyCon "Boolean" Star)

-- | Generate type constraints based on Damas-Hindley-Milner type rules to be
-- solved later.
generateConstraints :: Untyped -> TypeCheck (Type, TypeResult)

generateConstraints (() :< IntC _) = return (int_t, mempty)
generateConstraints (() :< DoubleC _) = return (float_t, mempty)
generateConstraints (() :< BoolC _) = return (bool_t, mempty)

generateConstraints (() :< IdC sym) = do
    defns <- gets definitions
    (TypeEnv te) <- gets typeEnv
    case M.lookup sym defns of
        Just u -> do
            (ty, tr) <- generateConstraints u
            let sc = closeOver ty
            return (ty, tr <> TypeResult {
                           constraints = [ ty :~ sc ],
                           assumptions = mempty })
        Nothing -> case M.lookup sym te of
            Just sc -> do
                var <- freshVarId
                return (var, TypeResult [ var :~ sc ] mempty)
            Nothing -> do
                var <- freshVarId
                return (var, TypeResult [] (M.singleton sym [var]))

generateConstraints (() :< FunC argSyms body) = do
    br <- memoizedTC generateConstraints body
    (vars, tr) <- foldM
        (\(vars, TypeResult cs as) arg -> do
                var@(TVar (TyVar n k)) <- freshVarId
                let cs' = maybe [] (map (var :=))
                          (M.lookup arg as)
                    as' = M.delete arg as
                return (vars <> [var], TypeResult {
                               constraints = cs' <> cs,
                               assumptions = as'
                               }))
        ([], TypeResult [] (assumptions $ snd br))
        argSyms
    return ((vars |-> (fst br)), TypeResult {
                   constraints = constraints (snd br) <> constraints tr,
                   assumptions = assumptions tr
                   })

generateConstraints (() :< AppC op erands) = do
    var <- freshVarId
    op' <- memoizedTC generateConstraints op
    (tys, erands') <- mapAndUnzipM (memoizedTC generateConstraints) erands
    let results = foldl (<>) mempty erands'
    let cs = [ (fst op') := (tys |-> var) ]
    return (var, snd op' <> results <> TypeResult {
                   constraints = cs,
                   assumptions = mempty
                   })

generateConstraints (() :< IfC c t e) = do
    var <- freshVarId
    cr <- memoizedTC generateConstraints c
    tr <- memoizedTC generateConstraints t
    er <- memoizedTC generateConstraints e
    return (var, snd cr <> snd tr <> snd er <> TypeResult {
                   constraints = [ (fst tr) := (fst er)
                                 , var := (fst tr)
                                 , (fst cr) := bool_t
                                 ],
                   assumptions = mempty
                   })

-- | Instantiating a type scheme replaces all of its type variables with new
-- unique variables. Each time a polymorphic definition is used it is
-- instantiated differently.
instantiate :: Scheme -> TypeCheck Type
instantiate (Forall vars (ps :=> t)) = do
    nvars <- mapM (\_ -> freshVarId) vars
    let f = Frame $ M.fromList (zip vars nvars)
    return $ substitute f t

-- | When a top level definition is inferred, its type is generalized into a
-- type scheme so that it may be instantiated multiple times later depending on
-- usage.
generalize :: TypeEnv -> Type -> Scheme
generalize te t = Forall as ([] :=> t)
    where as = S.toList $ ftv t `S.difference` ftv te

-- | Type schemes don't need to have globally unique type variables.
normalize :: Scheme -> Scheme
normalize (Forall _ (ps :=> t)) = Forall (map snd ord) (ps' :=> (normtype t))
    where
        ord = zip (nub $ fv t) (map (\n -> TyVar n Star) [1..])
        ps' = map (\(IsIn sym (TVar t)) -> maybe
                                    (error "fuck")
                                    (\x -> (IsIn sym (TVar x)))
                                    (Prelude.lookup t ord))
              ps
        fv = reverse . S.toList . ftv

        normtype (TFun ts) = TFun $ map normtype ts
        normtype (TSym sym) = TSym sym
        normtype (TVar tv) = case Prelude.lookup tv ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"

-- | Generalize and normalize a type into a type scheme in one smooth motion.
closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

-- * Unification and constraint solving

unify :: Type -> Type -> Maybe Frame -> Maybe Frame
unify t1 t2 (Just frame)
    | t1 == t2 = Just frame
    | otherwise = go t1 t2 where

          go (TVar _) t2 = unify_var t1 t2 frame
          go t1 (TVar _) = unify_var t2 t1 frame

          go (TFun (a:as)) (TFun (b:bs)) =
              unify (TFun as) (TFun bs) $
              unify (substitute frame a) (substitute frame b) $
              Just frame

          go (TSym a) (TSym b)
              | a == b = Just frame
              | otherwise = Nothing

          go _ _ = Nothing

unify _ _ Nothing = Nothing

unify_var :: Type -> Type -> Frame -> Maybe Frame
unify_var var@(TVar tv) val frame
    | var == val = Just frame
    | otherwise = case frameLookup frame tv of
                      Nothing -> if occurs_check var val frame
                                    then Just $ frameInsert frame tv val
                                    else Nothing

                      Just val' -> unify val' val $ Just frame

unify_var _ _ _ = Nothing

-- | Returns true if the first type is not used to define the second
-- type.
occurs_check :: Type -> Type -> Frame -> Bool
occurs_check var@(TVar x) val frame = go (substitute frame val)
    where go (TSym _) = True
          go (TVar y)
              | var == val = True
              | otherwise = case frameLookup frame y of
                                Nothing -> True
                                Just v' -> go v'

          go (TFun (x:xs)) = and [ go x, go (TFun xs) ]
          go (TFun []) = True

match :: Type -> Type -> Maybe Frame -> Maybe Frame
match _ _ Nothing = Nothing
match (TFun (t1:ts1)) (TFun (t2:ts2)) (Just frame) = do
    sl <- match t1 t2 (Just frame)
    sr <- match (TFun ts1) (TFun ts2) (Just frame)
    return $ sl <> sr

match (TVar u) t (Just frame)
    | kind u == kind t = return (frameInsert frame u t)

match (TSym c1) (TSym c2) (Just frame)
    | c1 == c2 = return mempty

match t1 t2 (Just frame) = fail "types do not match"

-- | The set of constraints is unified one by one until failure or a 'Frame' has
-- been constructed.
solveConstraints :: [Constraint] -> TypeCheck (Maybe Frame)
solveConstraints cs = go (Just mempty) cs where
    go result [] = return result
    go Nothing _ = return Nothing

    go f@(Just frame) ((a := b) : cs) = do
        let fr = unify (substitute frame a) (substitute frame b) $ Just frame
        go (fr <> f) cs

    go f@(Just frame) ((a :~ b) : cs) = do
        b' <- instantiate (substitute frame b)
        let fr = unify (substitute frame a) (substitute frame b') $ Just frame
        go (fr <> f) cs

modifyTypeEnv :: (TypeEnv -> TypeEnv) -> TypeCheck ()
modifyTypeEnv f = gets typeEnv >>= \te -> modify $ \st -> st { typeEnv = f te }

inferTop :: TypeEnv -> [(Symbol, Untyped)] -> IO (Maybe TypeEnv)
inferTop te [] = return $ Just te
inferTop te exprs = (flip evalStateT) ts $ do

    ccs <- forM exprs $ \(name, expr) -> do
        (ty, tr) <- memoizedTC generateConstraints expr
        modifyTypeEnv $ \te -> envInsert te name $ Forall [] ([] :=> ty)
        return $ constraints tr

    let cs = concat ccs
    mFrame <- solveConstraints cs
    case mFrame of
        Nothing -> return Nothing
        Just frame -> do
            (TypeEnv te) <- gets typeEnv
            let (TypeEnv te') = substitute frame (TypeEnv te)
            return . Just . TypeEnv . M.map normalize $ te'

    where ts = newTypeState {
              typeEnv = te,
              definitions = M.fromList exprs }

-- * Informal hobby-project-chic testing routines.
num_pred = (IsIn "Num" (TVar (TyVar 0 Star)))
num_type = TVar (TyVar 0 Star)
mul_type = Forall [TyVar 0 Star] ([num_pred] :=> ([num_type, num_type] |-> num_type))
add_type = mul_type
te = TypeEnv $ M.fromList [("*", mul_type), ("+", add_type)]
test :: IO ()
test = do
    defns <- parse_multi $ T.concat
        [ "(defun times-1 (x) (* 1 x))"
--        , "(def nine (square 3))"
        , "(defun id (z) z)"
        , "(defun square (y) (* y y))"
--        , "(def four (id (square 2)))"
        , "(defun box (b) (\\ (f) (id (f b))))" -- sanity check
        , "(defun pair (x y) (\\ (f) (f x y)))"
        ]
    let defns' = map (\(Free (DefC sym expr)) -> (sym, untyped expr)) defns
    results <- inferTop te defns'
    case results of
        Nothing -> putStrLn "Typecheck failed!"
        Just (TypeEnv te) -> do
            forM_ (M.toList te) $ \(name, ty) ->
                putStrLn $ name ++ " : " ++ (show ty)
