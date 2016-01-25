{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Oden.Infer (
  Constraint,
  TypeError(..),
  Subst(..),
  inferExpr,
  inferDefinition,
  inferPackage,
  constraintsExpr
) where

import           Control.Arrow          (left)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS      hiding ((<>))

import           Data.List              (nub)
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Set               as Set

import qualified Oden.Core              as Core
import qualified Oden.Core.Untyped      as Untyped
import           Oden.Env               as Env
import           Oden.Identifier
import           Oden.Infer.Substitution
import           Oden.Infer.Subsumption
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


instance FTV Constraint where
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)

instance FTV Env where
  ftv (TypeEnv env) = ftv $ Map.elems env

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | NotInScope Identifier
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | ArgumentCountMismatch [Type] [Type]
  | TypeSignatureSubsumptionError Name SubsumptionError
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
    return (Core.Fn a tb (tv `TFn` Core.typeOf tb))

  Untyped.NoArgFn f -> do
    tf <- infer f
    return (Core.NoArgFn tf (TNoArgFn (Core.typeOf tf)))

  Untyped.Application f [] -> do
    tf <- infer f
    tv <- fresh
    case Core.typeOf tf of
      t@(TUncurriedFn _ _) -> do
        uni t (TUncurriedFn [] tv)
        return (Core.UncurriedFnApplication tf [] tv)
      -- No-param application of variadic function is automatically transformed
      -- to application of empty slice.
      t@(TVariadicFn [] variadicArg _) -> do
        uni t (TVariadicFn [] variadicArg tv)
        return (Core.UncurriedFnApplication tf [Core.Slice [] variadicArg] tv)
      (TVariadicFn nonVariadicArgs _ _) ->
        throwError (ArgumentCountMismatch nonVariadicArgs [])
      t -> do
        uni t (TNoArgFn tv)
        return (Core.NoArgApplication tf tv)

  Untyped.Application f ps -> do
    tf <- infer f
    case Core.typeOf tf of
      t@(TUncurriedFn _ _) -> do
        tv <- fresh
        tps <- mapM infer ps
        uni t (TUncurriedFn (map Core.typeOf tps) tv)
        return (Core.UncurriedFnApplication tf tps tv)
      t@(TVariadicFn nonVariadicTypes variadicType _) -> do
        tv <- fresh
        nonVariadicParams <- mapM infer (take (length nonVariadicTypes) ps)
        variadicParams <- mapM infer (drop (length nonVariadicTypes) ps)
        let allParams = nonVariadicParams ++ [Core.Slice variadicParams variadicType]
        uni t (TVariadicFn (map Core.typeOf nonVariadicParams) variadicType tv)
        return (Core.UncurriedFnApplication tf allParams tv)
      _ ->
        foldM app tf ps
        where
        app :: Core.Expr Type -> Untyped.Expr -> Infer (Core.Expr Type)
        app tf' p = do
          tv <- fresh
          tp <- infer p
          uni (Core.typeOf tf') (Core.typeOf tp `TFn` tv)
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
inferDef (Untyped.Definition name s expr) = do
  tv <- fresh
  env <- ask
  let recType = fromMaybe (Forall [] tv) s
  te <- inEnv (Unqualified name, recType) (infer expr)
  return (Core.Definition name (generalize env te))

inferDefinition :: Env -> Untyped.Definition -> Either TypeError Core.Definition
inferDefinition env def@(Untyped.Definition _ Nothing _) = do
  (Core.Definition name (_, te), cs) <- runInfer env (inferDef def)
  subst <- runSolve cs
  return $ Core.Definition name (closeOver (apply subst te))
inferDefinition env def@(Untyped.Definition _ (Just st) _) = do
  (Core.Definition name ce, cs) <- runInfer env (inferDef def)
  subst <- runSolve cs
  let (Forall _ _, substExpr) = apply subst ce
  ce' <- left (TypeSignatureSubsumptionError name) $ subsumeTypeSignature st substExpr
  return $ Core.Definition name ce'

inferPackage :: Env -> Untyped.Package -> Either TypeError (Core.Package, Env)
inferPackage env (Untyped.Package name imports defs) = do
  (env', inferred) <- foldM iter (env, []) defs
  return (Core.Package name (map convertImport imports) inferred, env')
  where
  iter (e, inferred) d@Untyped.Definition{} = do
      td@(Core.Definition n (sc, _)) <- inferDefinition e d
      return (extend e (Unqualified n, sc), inferred ++ [td])
  convertImport (Untyped.Import n) = Core.Import n

normalize :: (Scheme, Core.Expr Type) -> (Scheme, Core.Expr Type)
normalize (Forall _ body, te) = (Forall (map snd ord) (normtype body), te)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv TAny           = []
    fv (TVar a)       = [a]
    fv (TNoArgFn a) = fv a
    fv (TFn a b)     = fv a ++ fv b
    fv (TUncurriedFn as r) = concatMap fv as ++ fv r
    fv (TVariadicFn as v r) = concatMap fv as ++ fv v ++ fv r
    fv (TCon _)       = []
    fv (TSlice t)     = fv t

    normtype TAny           = TAny
    normtype (TNoArgFn a) = TNoArgFn (normtype a)
    normtype (TFn a b)     = TFn (normtype a) (normtype b)
    normtype (TUncurriedFn as r) = TUncurriedFn (map normtype as) (normtype r)
    normtype (TVariadicFn as v r) = TVariadicFn (map normtype as) (normtype v) (normtype r)
    normtype (TCon a)       = TCon a
    normtype (TSlice a)     = TSlice (normtype a)
    normtype (TVar a)       =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

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
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TFn t1 t2) (TFn t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TNoArgFn t1) (TNoArgFn t2) = unifies t1 t2
unifies (TUncurriedFn as1 r1) (TUncurriedFn as2 r2) = do
  a <- unifyMany as1 as2
  r <- unifies r1 r2
  return (a `compose` r)
unifies (TVariadicFn as1 v1 r1) (TVariadicFn as2 v2 r2) = do
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
