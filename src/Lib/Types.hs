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
import qualified Data.Text as T

import Lib.Syntax
import Lib.Parser (parse_expr, parse_multi)
import Lib.Util



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
(|->) :: [ Qual Type ] -> Qual Type -> Qual Type
args |-> (psb :=> body) = ps :=> (TFun $ args' ++ [body])
    where (psa, args') = unzip $ map (\(pa :=> arg) -> (pa, arg)) args
          ps = sort $ (concat psa) ++ psb

-- | A mapping from type variables to concrete types. The goal of type inference
-- is to construct a frame for a given program that is consistent and sound.
newtype Frame = Frame {
    bindings :: Map TyVar (Qual Type)
    } deriving (Eq, Show)

instance Monoid Frame where
    mempty = Frame mempty
    (Frame f1) `mappend` (Frame f2) = Frame $
        M.map (substitute (Frame f1)) f2 `M.union` f1

frameLookup :: Frame -> TyVar -> Maybe (Qual Type)
frameLookup (Frame frame) i = M.lookup i frame

frameInsert :: Frame -> TyVar -> (Qual Type) -> Frame
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

    substitute frame v@(TVar n) =
        maybe v (\(_ :=> t) -> substitute frame t) $
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
(f <:> g) ce = f ce >>= g
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

initialInstances :: EnvTransformer
initialInstances = addPreludeClasses
           <:> addInst [] (IsIn "Show" int_t)
           <:> addInst [] (IsIn "Eq" int_t)
           <:> addInst [] (IsIn "Ord" int_t)
           <:> addInst [] (IsIn "Num" int_t)
           <:> addInst [] (IsIn "Integral" int_t)
           <:> addInst [] (IsIn "Show" float_t)
           <:> addInst [] (IsIn "Eq" float_t)
           <:> addInst [] (IsIn "Ord" float_t)
           <:> addInst [] (IsIn "Num" float_t)
           <:> addInst [] (IsIn "Fractional" float_t)
           <:> addInst [] (IsIn "Show" bool_t)
           <:> addInst [] (IsIn "Eq" bool_t)

baseClassEnv :: Maybe ClassEnv
baseClassEnv = initialInstances initialClassEnv

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

-- * Type schemes

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

instance Typing Scheme where
    ftv (Forall vars t) = (ftv t) `S.difference` (S.fromList vars)

    substitute (Frame frame) (Forall vars t) = Forall vars $
        substitute (Frame $ foldr M.delete frame vars) t

-- * Constraint generation

-- | If two types are constrained then there must be a valid unification for
-- them. Inference fails unless all constraints are satisfied.
data Constraint
    = Qual Type := Qual Type -- ^ equality constraint
    | Qual Type :~ Scheme -- ^ instance constraint
    deriving (Show, Eq, Ord)

-- | At each step of inference we will have generated constraints along with
-- assumptions about symbols we have seen. The assumptions are used when typing
-- functions.
data TypeResult = TypeResult
    { constraints :: [ Constraint ]
    , assumptions :: Map String [ Qual Type ]
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
    , memo :: Map Untyped (Qual Type, TypeResult)
    , typeEnv :: TypeEnv
    , definitions :: Map Symbol Untyped
    }

newTypeState :: TypeState
newTypeState = TypeState 0 mempty mempty mempty

-- | A simple type checking monad. It doesn't need to be based on IO but is for
-- lazy debugging purposes right now.
type TypeCheck = StateT TypeState IO

-- | You get a globally unique type variable each time you call this function.
freshVarId :: Kind -> TypeCheck (Qual Type)
freshVarId k = do
    v <- gets varId
    modify $ \s -> s { varId = succ v }
    return $ [] :=> (TVar (TyVar v k))

-- | The actual memoization function for the 'TypeCheck' monad.
memoizedTC
    :: (Untyped -> TypeCheck (Qual Type, TypeResult))
    -> Untyped
    -> TypeCheck (Qual Type, TypeResult)
memoizedTC f c@(() :< c') = gets memo >>= maybe memoize return . M.lookup c
    where memoize = do
              r <- f c
              modify $ \s -> s { memo = M.insert c r $ memo s }
              return r
{-
    where memoize = case c' of
                        IdC _ -> f c
                        _ -> do
                            r <- f c
                            modify $ \s -> s { memo = M.insert c r $ memo s }
                            return r
-}

-- | Some basic helper types, which should probably be treated a little more
-- formally.
int_t = TSym (TyCon "Int" Star)
float_t = TSym (TyCon "Float" Star)
bool_t = TSym (TyCon "Boolean" Star)
int_p = [IsIn "Integral" $ TVar $ TyVar 1000 Star] :=> (TVar (TyVar 1000 Star))
float_p = [IsIn "Fractional" $ TVar $ TyVar 1001 Star] :=> (TVar (TyVar 1001 Star))

-- | Generate type constraints based on Damas-Hindley-Milner type rules to be
-- solved later.
generateConstraints :: Untyped -> TypeCheck (Qual Type, TypeResult)

generateConstraints (() :< IntC _) = do
    (_ :=> t) <- freshVarId Star
    return ([IsIn "Integral" t] :=> t, mempty)
generateConstraints (() :< DoubleC _) = do
    (_ :=> t) <- freshVarId Star
    return ([IsIn "Fractional" t] :=> t, mempty)
generateConstraints (() :< BoolC _) = return ([] :=> bool_t, mempty)

generateConstraints (() :< IdC sym) = do
    defns <- gets definitions
    (TypeEnv te) <- gets typeEnv
    var <- freshVarId Star
    case M.lookup sym defns of
        Just u -> do
            (ty, tr) <- memoizedTC generateConstraints u
            let sc = closeOver ty
            return (var, tr <> TypeResult {
                           constraints = [ var := ty, ty :~ sc ],
                           assumptions = mempty })
        Nothing -> case M.lookup sym te of
            Just sc -> do
                ty <- instantiate sc
                return (var, TypeResult [ var := ty, ty :~ sc ] mempty)
            Nothing -> do
                var <- freshVarId Star
                return (var, TypeResult [] (M.singleton sym [var]))

generateConstraints (() :< FunC argSyms body) = do
    br <- memoizedTC generateConstraints body
    (vars, tr) <- foldM
        (\(vars, TypeResult cs as) arg -> do
                var <- freshVarId Star
                let cs' = maybe [] (map (var :=))
                          (M.lookup arg as)
                    as' = M.delete arg as
                return (vars <> [var], TypeResult {
                               constraints = cs' <> cs,
                               assumptions = as'
                               }))
        ([], TypeResult [] (assumptions $ snd br))
        argSyms

    let (ps :=> fun_t) = vars |-> (fst br)
    ps' <- reduce (fromJust baseClassEnv) ps
    return (ps' :=> fun_t , TypeResult {
                   constraints = constraints (snd br) <> constraints tr,
                   assumptions = assumptions tr
                   })

generateConstraints (() :< AppC op erands) = do
    var <- freshVarId Star
    op' <- memoizedTC generateConstraints op
    (tys, erands') <- mapAndUnzipM (memoizedTC generateConstraints) erands
    let results = foldl (<>) mempty erands'
    let (ps :=> fun_t) = tys |-> var
    ps' <- reduce (fromJust baseClassEnv) ps
    let cs = [ (fst op') := (ps' :=> fun_t) ]
    return (var, snd op' <> results <> TypeResult {
                   constraints = cs,
                   assumptions = mempty
                   })

generateConstraints (() :< IfC c t e) = do
    var <- freshVarId Star
    cr <- memoizedTC generateConstraints c
    tr <- memoizedTC generateConstraints t
    er <- memoizedTC generateConstraints e
    return (var, snd cr <> snd tr <> snd er <> TypeResult {
                   constraints = [ (fst tr) := (fst er)
                                 , var := (fst tr)
                                 , (fst cr) := ([] :=> bool_t)
                                 ],
                   assumptions = mempty
                   })

-- | Instantiation is the process of replacing type variables with a set of new
-- variables which allows a polymorphic value to be correctly inferred depending
-- on context.
class Instantiate t where
    inst :: [Type] -> t -> t

instance Instantiate Type where
    inst is (TFun ts) = TFun $ inst is ts
    inst is (TVar (TyVar n _)) = is !! n
    inst [] t = t
    inst is t = t

instance Instantiate t => Instantiate [t] where
    inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
    inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)

instantiate :: Scheme -> TypeCheck (Qual Type)
instantiate (Forall vars (ps :=> t)) = do
    ps' <- reduce (fromJust baseClassEnv) ps
    qs <- mapM (\(TyVar _ k) -> freshVarId k) vars
    let nvars = map (\(_ :=> var) -> var) qs
    let f = Frame $ M.fromList (zip vars {- nvars -} qs)
    let (ps'' :=> t') = inst (substitute f nvars) $ ps' :=> t
    return $ (substitute f ps'' :=> substitute f t')

-- | When a top level definition is inferred, its type is generalized into a
-- type scheme so that it may be instantiated multiple times later depending on
-- usage.
-- NB keep an eye out for this one
generalize :: TypeEnv -> Qual Type -> Scheme
generalize te t = Forall as t
    where as = S.toList $ ftv t `S.difference` ftv te

-- | Type schemes don't need to have globally unique type variables.
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

-- | Generalize and normalize a type into a type scheme in one smooth motion.
closeOver :: Qual Type -> Scheme
closeOver = normalize . generalize mempty

-- * Unification and constraint solving

mgu :: Type -> Type -> Maybe Frame
mgu t1 t2 = unify t1 t2 $ Just mempty

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
                                    then Just $ frameInsert frame tv
                                         ([] :=> val)
                                    else Nothing

                      Just (_ :=> val') -> unify val' val $ Just frame

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
                                Just (_ :=> v') -> go v'

          go (TFun (x:xs)) = and [ go x, go (TFun xs) ]
          go (TFun []) = True

match :: Type -> Type -> Maybe Frame -> Maybe Frame
match _ _ Nothing = Nothing
match (TFun (t1:ts1)) (TFun (t2:ts2)) (Just frame) = do
    sl <- match t1 t2 (Just frame)
    sr <- match (TFun ts1) (TFun ts2) (Just frame)
    return $ sl <> sr

match (TVar u) t (Just frame)
    | kind u == kind t = return (frameInsert frame u ([] :=> t))

match (TSym c1) (TSym c2) (Just frame)
    | c1 == c2 = return mempty

match t1 t2 (Just frame) = fail "types do not match"

-- | The set of constraints is unified one by one until failure or a 'Frame' has
-- been constructed.
solveConstraints :: Maybe ClassEnv -> [Constraint] -> TypeCheck (Maybe Frame)
solveConstraints Nothing _ = return Nothing
solveConstraints (Just ce) cs = go (Just mempty) cs where
    go result [] = return result
    go Nothing _ = return Nothing

    go f@(Just frame) ((a@(psa :=> ta) := b@(psb :=> tb)) : cs) = do
--        liftIO . putStrLn $ show a ++ " = " ++ show b
        let a'@(psa' :=> ta') = substitute frame a
        let b'@(psb' :=> tb') = substitute frame b
        let frame' = unify ta' tb' f
        let tyvars = S.toList $ ftv ta'
        let f' = frame' >>= restore_preds psb' tyvars

        go (f' <> f) cs

    go f@(Just frame) (((psa :=> a) :~ b) : cs) = do
--        liftIO . putStrLn $ show a ++ " ~ " ++ show b
        (psb :=> b') <- instantiate (substitute frame b)
        let frame' = unify (substitute frame a) (substitute frame b') f
        let tyvars = S.toList $ ftv $ psa :=> a
        let f' = frame' >>= restore_preds psb tyvars
        go (f' <> f) cs

    go f _ = return f

restore_preds :: [Pred] -> [TyVar] -> Frame -> Maybe Frame
restore_preds ps tvs frame = foldM go frame tvs
    where pred_map = M.fromList $ map (\p@(IsIn _ ty) -> (ty, p)) ps
          go fr tyvar = do
              (ps :=> bound_ty) <- frameLookup fr tyvar
              case M.lookup bound_ty pred_map of
                  Nothing -> return fr
                  Just p -> return $ frameInsert fr tyvar ((p:ps) :=> bound_ty)

modifyTypeEnv :: (TypeEnv -> TypeEnv) -> TypeCheck ()
modifyTypeEnv f = gets typeEnv >>= \te -> modify $ \st -> st { typeEnv = f te }

invert :: Frame -> Map TyVar [Pred]
invert (Frame f) = foldl go M.empty $ map snd $ M.toList f
    where go m (ps :=> ty)
              | is_var ty = let (TVar tv) = ty
                            in  case M.lookup tv m of
                                    Nothing -> M.insert tv ps m
                                    Just _ -> M.adjust (++ ps) tv m
              | otherwise = m

inferTop :: TypeEnv -> [(Symbol, Untyped)] -> IO (Maybe TypeEnv)
inferTop te [] = return $ Just te
inferTop te exprs = (flip evalStateT) ts $ do

    results <- forM (map snd exprs) $
        sequence . extend (memoizedTC generateConstraints)

    constrained <- forM (zip (fst <$> exprs) results) $ \(sym, result) -> do
        let (ty, tr) = extract result
        return $ (sym, ty, sort $ nub $ constraints tr)

    liftIO . putStrLn $ "---"

    forM_ constrained $ \(sym, ty, cs) -> do
        mFrame <- solveConstraints baseClassEnv cs
        case mFrame of
            Nothing -> liftIO . putStrLn $ sym ++ " failed"
            Just frame -> do
                let (ps :=> ty') = substitute frame ty
                let inverted = invert frame
                let fvs = S.toList $ ftv ty'
                let ps' = concat $ map (\fv -> maybe [] id (M.lookup fv inverted)) fvs
                preds <- reduce (fromJust baseClassEnv) (ps ++ ps')
                let sc = closeOver $ preds :=> ty'
                liftIO . putStrLn $ sym ++ " : " ++ show sc

--        forM_ cs $ liftIO . putStrLn . show
        liftIO . putStrLn $ "---"

    return Nothing

    where ts = newTypeState {
                   varId = 1,
                   typeEnv = te,
                   definitions = M.fromList exprs }

is_var :: Type -> Bool
is_var (TVar _) = True
is_var _ = False

-- * Informal hobby-project-chic testing routines.
num_pred = (IsIn "Num" (TVar (TyVar 0 Star)))
num_type = [num_pred] :=> (TVar (TyVar 0 Star))

test :: IO ()
test = do
    let (ps :=> mult_t) = [num_type, num_type] |-> num_type
    ps' <- reduce (fromJust baseClassEnv) ps
    let mul_type = Forall [TyVar 0 Star] (ps' :=> mult_t)
    let add_type = mul_type
    let te = TypeEnv $ M.fromList [("*", mul_type) {-, ("+", add_type) -} ]

    defns <- parse_multi $ T.concat
        [ "(defun square (y) (* y y))"
        , "(def two 2)"
        , "(def four (square two))"
        , "(def ocho (* two 4))"
        , "(def three-and-a-half 3.5)"
        , "(def seven (* three-and-a-half 2.0))"
        , "(def wut (square three-and-a-half))"
        , "(defun compose (f g) (\\ (x) (f (g x))))"
        ]

    let defns' = map (\(Free (DefC sym expr)) -> (sym, untyped expr)) defns
    results <- inferTop te defns'
    putStrLn "-----"
    case results of
        Nothing -> putStrLn "Typecheck failed!"
        Just (TypeEnv te) -> do
            forM_ (M.toList te) $ \(name, ty) ->
                putStrLn $ name ++ " : " ++ (show ty)

{-
[1]: When you use `closeOver` here you end up with, eg, id having two distinct
type variables
-}
