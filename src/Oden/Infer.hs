{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oden.Infer (
  Constraint,
  TypeError(..),
  Subst(..),
  inferExpr,
  inferPackage,
  constraintsExpr
) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Oden.Identifier
import Oden.Env as Env
import Oden.Type.Polymorphic
import qualified Oden.Core.Untyped as Untyped
import qualified Oden.Core as Core

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
  apply _ (TCon a)            = TCon a
  apply (Subst s) t@(TVar a)  = Map.findWithDefault t a s
  apply s (TArrSingle t)      = TArrSingle (apply s t)
  apply s (t1 `TArr` t2)      = apply s t1 `TArr` apply s t2

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (TArrSingle t) = ftv t

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
  apply s (Core.Symbol x t)           = Core.Symbol x (apply s t)
  apply s (Core.Application f p t)    = Core.Application (apply s f) (apply s p) (apply s t)
  apply s (Core.NoArgApplication f t) = Core.NoArgApplication (apply s f) (apply s t)
  apply s (Core.Fn x b t)             = Core.Fn x (apply s b) (apply s t)
  apply s (Core.NoArgFn b t)          = Core.NoArgFn (apply s b) (apply s t)
  apply s (Core.Let x e b t)          = Core.Let x (apply s e) (apply s b) (apply s t)
  apply s (Core.Literal l t)          = Core.Literal l (apply s t)
  apply s (Core.If c tb fb t)         = Core.If (apply s c) (apply s tb) (apply s fb) (apply s t)
  apply s (Core.Fix e t)              = Core.Fix (apply s e) (apply s t)

  ftv = ftv . Core.typeOf

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundIdentifier Identifier
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  deriving (Eq, Show)

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
      Nothing   ->  throwError $ UnboundIdentifier x
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

  Untyped.Application f p -> do
    tf <- infer f
    tp <- infer p
    tv <- fresh
    uni (Core.typeOf tf) (Core.typeOf tp `TArr` tv)
    return (Core.Application tf tp tv)

  Untyped.NoArgApplication f -> do
    tf <- infer f
    tv <- fresh
    uni (Core.typeOf tf) (TArrSingle tv)
    return (Core.NoArgApplication tf tv)

  Untyped.Let n e b -> do
    env <- ask
    te <- infer e
    let (sc, te') = generalize env te
    tb <- inEnv (Unqualified n, sc) (infer b)
    return (Core.Let n te' tb (Core.typeOf tb))

  Untyped.Fix ex -> do
    te <- infer ex
    tv <- fresh
    uni (tv `TArr` tv) (Core.typeOf te)
    return (Core.Fix te tv)

  Untyped.If cond tr fl -> do
    tcond <- infer cond
    ttr <- infer tr
    tfl <- infer fl
    uni (Core.typeOf tcond) typeBool
    uni (Core.typeOf ttr) (Core.typeOf tfl)
    return (Core.If tcond ttr tfl (Core.typeOf ttr))

inferDef :: Untyped.Definition -> Infer Core.Definition
inferDef (Untyped.Definition name expr) = do
  tv <- fresh
  env <- ask
  te <- inEnv (Unqualified name, Forall [] tv) (infer expr)
  return (Core.Definition name (generalize env te))

inferDefinition :: Env -> Untyped.Definition -> Either TypeError Core.Definition
inferDefinition env def = do
  (Core.Definition name (sc, te), cs) <- runInfer env (inferDef def)
  subst <- runSolve cs
  return $ Core.Definition name (closeOver (apply subst te))

inferPackage :: Env -> Untyped.Package -> Either TypeError (Core.Package, Env)
inferPackage env (Untyped.Package name imports defs) = do
  (env', inferred) <- foldM iter (env, []) defs
  return (Core.Package name (map convertImport imports) inferred, env')
  where
  iter (env, inferred) d@(Untyped.Definition name _) = do
      td@(Core.Definition name (sc, te)) <- inferDefinition env d
      return (extend env (Unqualified name, sc), inferred ++ [td])
  convertImport (Untyped.Import name) = Core.Import name

normalize :: (Scheme, Core.Expr Type) -> (Scheme, Core.Expr Type)
normalize (Forall _ body, te) = (Forall (map snd ord) (normtype body), te)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)       = [a]
    fv (TArrSingle a) = fv a
    fv (TArr a b)     = fv a ++ fv b
    fv (TCon _)       = []

    normtype (TArrSingle a) = TArrSingle (normtype a)
    normtype (TArr a b)     = TArr (normtype a) (normtype b)
    normtype (TCon a)       = TCon a
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
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TArrSingle t1) (TArrSingle t2) = unifies t1 t2
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
