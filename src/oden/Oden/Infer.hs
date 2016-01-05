{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Oden.Infer (
  Constraint,
  TypeError(..),
  Subst(..),
  inferExpr,
  inferPackage,
  constraintsExpr
) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS      hiding ((<>))

import           Data.List              (nub)
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import qualified Oden.Core              as Core
import qualified Oden.Core.Untyped      as Untyped
import           Oden.Env               as Env
import           Oden.Identifier
import           Oden.Type.Polymorphic

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer a = (RWST
                  Env             -- Typing environment
                  [Constraint]    -- Generated constraints
                  InferState      -- Inference state
                  (Except         -- Inference errors
                    TypeError)
                  a)              -- Result

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ TAny                      = TAny
  apply _ (TCon a)                  = TCon a
  apply (Subst s) t@(TVar a)        = Map.findWithDefault t a s
  apply s (TArrSingle t)            = TArrSingle (apply s t)
  apply s (t1 `TArr` t2)            = apply s t1 `TArr` apply s t2
  apply s (TGoFunc as r)            = TGoFunc (map (apply s) as) (apply s r)
  apply s (TVariadicGoFunc as v r)  = TVariadicGoFunc (map (apply s) as) (apply s v) (apply s r)
  apply s (TSlice t)                = TSlice (apply s t)

  ftv TAny                      = Set.empty
  ftv TCon{}                    = Set.empty
  ftv (TVar a)                  = Set.singleton a
  ftv (t1 `TArr` t2)            = ftv t1 `Set.union` ftv t2
  ftv (TArrSingle t)            = ftv t
  ftv (TGoFunc as r)            = foldl Set.union (ftv r) (map ftv as)
  ftv (TVariadicGoFunc as v r)  = foldl Set.union (ftv r) (ftv v : map ftv as)
  ftv (TSlice t)                = ftv t

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as


instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

instance Substitutable (Core.Expr Type) where
  apply s (Core.Symbol x t)               = Core.Symbol x (apply s t)
  apply s (Core.Application f p t)        = Core.Application (apply s f) (apply s p) (apply s t)
  apply s (Core.NoArgApplication f t)     = Core.NoArgApplication (apply s f) (apply s t)
  apply s (Core.GoFuncApplication f p t)  = Core.GoFuncApplication (apply s f) (apply s p) (apply s t)
  apply s (Core.Fn x b t)                 = Core.Fn x (apply s b) (apply s t)
  apply s (Core.NoArgFn b t)              = Core.NoArgFn (apply s b) (apply s t)
  apply s (Core.Let x e b t)              = Core.Let x (apply s e) (apply s b) (apply s t)
  apply s (Core.Literal l t)              = Core.Literal l (apply s t)
  apply s (Core.If c tb fb t)             = Core.If (apply s c) (apply s tb) (apply s fb) (apply s t)
  apply s (Core.Slice es t)               = Core.Slice es (apply s t)

  ftv = ftv . Core.typeOf

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | NotInScope Identifier
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | ArgumentCountMismatch [Type] [Type]
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer a -> Either TypeError (a, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Untyped.Expr -> Either TypeError Core.CanonicalExpr
inferExpr env ex = do
  (te, cs) <- runInfer env (infer ex)
  subst <- runSolve cs
  return $ closeOver (apply subst te)

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Untyped.Expr -> Either TypeError ([Constraint], Subst, Core.Expr Type, Scheme)
constraintsExpr env ex = do
  (te, cs) <- runInfer env (infer ex)
  subst <- runSolve cs
  let (sc, te') = closeOver $ apply subst te
  return (cs, subst, te', sc)

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Core.Expr Type -> Core.CanonicalExpr
closeOver = normalize . generalize Env.empty

-- | Unify two types
uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

-- | Extend type environment
inEnv :: (Identifier, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Identifier -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ NotInScope x
      Just s    ->  instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Core.Expr Type -> (Scheme, Core.Expr Type)
generalize env t  = (Forall as (Core.typeOf t), t)
    where as = Set.toList $ ftv t `Set.difference` ftv env

infer :: Untyped.Expr -> Infer (Core.Expr Type)
infer expr = case expr of
  Untyped.Literal (Untyped.Int n)  ->
    return (Core.Literal (Core.Int n) typeInt)
  Untyped.Literal (Untyped.Bool b) ->
    return (Core.Literal (Core.Bool b) typeBool)
  Untyped.Literal (Untyped.String s) ->
    return (Core.Literal (Core.String s) typeString)

  Untyped.Symbol x -> do
    t <- lookupEnv x
    return $ Core.Symbol x t

  Untyped.Fn a b -> do
    tv <- fresh
    tb <- inEnv (Unqualified a, Forall [] tv) (infer b)
    return (Core.Fn a tb (tv `TArr` Core.typeOf tb))

  Untyped.NoArgFn f -> do
    tf <- infer f
    return (Core.NoArgFn tf (TArrSingle (Core.typeOf tf)))

  Untyped.Application f [] -> do
    tf <- infer f
    tv <- fresh
    case Core.typeOf tf of
      t@(TGoFunc _ _) -> do
        uni t (TGoFunc [] tv)
        return (Core.GoFuncApplication tf [] tv)
      -- No-param application of variadic function is automatically transformed
      -- to application of empty slice.
      t@(TVariadicGoFunc [] variadicArg _) -> do
        uni t (TVariadicGoFunc [] variadicArg tv)
        return (Core.GoFuncApplication tf [Core.Slice [] variadicArg] tv)
      (TVariadicGoFunc nonVariadicArgs _ _) ->
        throwError (ArgumentCountMismatch nonVariadicArgs [])
      t -> do
        uni t (TArrSingle tv)
        return (Core.NoArgApplication tf tv)

  Untyped.Application f ps -> do
    tf <- infer f
    case Core.typeOf tf of
      t@(TGoFunc _ _) -> do
        tv <- fresh
        tps <- mapM infer ps
        uni t (TGoFunc (map Core.typeOf tps) tv)
        return (Core.GoFuncApplication tf tps tv)
      t@(TVariadicGoFunc nonVariadicTypes variadicType _) -> do
        tv <- fresh
        nonVariadicParams <- mapM infer (take (length nonVariadicTypes) ps)
        variadicParams <- mapM infer (drop (length nonVariadicTypes) ps)
        let allParams = nonVariadicParams ++ [Core.Slice variadicParams variadicType]
        uni t (TVariadicGoFunc (map Core.typeOf nonVariadicParams) variadicType tv)
        return (Core.GoFuncApplication tf allParams tv)
      _ ->
        foldM iter tf ps
        where
        iter :: Core.Expr Type -> Untyped.Expr -> Infer (Core.Expr Type)
        iter tf' p = do
          tv <- fresh
          tp <- infer p
          uni (Core.typeOf tf') (Core.typeOf tp `TArr` tv)
          return (Core.Application tf' tp tv)

  Untyped.Let n e b -> do
    te <- infer e
    tb <- inEnv (Unqualified n, Forall [] (Core.typeOf te)) (infer b)
    return (Core.Let n te tb (Core.typeOf tb))

  Untyped.If cond tr fl -> do
    tcond <- infer cond
    ttr <- infer tr
    tfl <- infer fl
    uni (Core.typeOf tcond) typeBool
    uni (Core.typeOf ttr) (Core.typeOf tfl)
    return (Core.If tcond ttr tfl (Core.typeOf ttr))

  Untyped.Slice es -> do
    tv <- fresh
    tes <- mapM infer es
    mapM_ (uni tv . Core.typeOf) tes
    return (Core.Slice tes (TSlice tv))

inferDef :: Untyped.Definition -> Infer Core.Definition
inferDef (Untyped.Definition name expr) = do
  tv <- fresh
  env <- ask
  te <- inEnv (Unqualified name, Forall [] tv) (infer expr)
  return (Core.Definition name (generalize env te))

inferDefinition :: Env -> Untyped.Definition -> Either TypeError Core.Definition
inferDefinition env def = do
  (Core.Definition name (_, te), cs) <- runInfer env (inferDef def)
  subst <- runSolve cs
  return $ Core.Definition name (closeOver (apply subst te))

inferPackage :: Env -> Untyped.Package -> Either TypeError (Core.Package, Env)
inferPackage env (Untyped.Package name imports defs) = do
  (env', inferred) <- foldM iter (env, []) defs
  return (Core.Package name (map convertImport imports) inferred, env')
  where
  iter (e, inferred) d@(Untyped.Definition _ _) = do
      td@(Core.Definition n (sc, _)) <- inferDefinition e d
      return (extend env (Unqualified n, sc), inferred ++ [td])
  convertImport (Untyped.Import n) = Core.Import n

normalize :: (Scheme, Core.Expr Type) -> (Scheme, Core.Expr Type)
normalize (Forall _ body, te) = (Forall (map snd ord) (normtype body), te)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv TAny           = []
    fv (TVar a)       = [a]
    fv (TArrSingle a) = fv a
    fv (TArr a b)     = fv a ++ fv b
    fv (TGoFunc as r) = concatMap fv as ++ fv r
    fv (TVariadicGoFunc as v r) = concatMap fv as ++ fv v ++ fv r
    fv (TCon _)       = []
    fv (TSlice t)     = fv t

    normtype TAny           = TAny
    normtype (TArrSingle a) = TArrSingle (normtype a)
    normtype (TArr a b)     = TArr (normtype a) (normtype b)
    normtype (TGoFunc as r) = TGoFunc (map normtype as) (normtype r)
    normtype (TVariadicGoFunc as v r) = TVariadicGoFunc (map normtype as) (normtype v) (normtype r)
    normtype (TCon a)       = TCon a
    normtype (TSlice a)     = TSlice (normtype a)
    normtype (TVar a)       =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies TAny (TVar v) = v `bind` TAny
unifies (TVar v) TAny = v `bind` TAny
unifies TAny _ = return emptySubst
unifies _ TAny = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TArrSingle t1) (TArrSingle t2) = unifies t1 t2
unifies (TGoFunc as1 r1) (TGoFunc as2 r2) = do
  a <- unifyMany as1 as2
  r <- unifies r1 r2
  return (a `compose` r)
unifies (TVariadicGoFunc as1 v1 r1) (TVariadicGoFunc as2 v2 r2) = do
  a <- unifyMany as1 as2
  v <- unifies v1 v2
  r <- unifies r1 r2
  return (a `compose` v `compose` r)
unifies (TSlice t1) (TSlice t2) = unifies t1 t2
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
