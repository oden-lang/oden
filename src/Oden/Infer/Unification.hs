{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Oden.Infer.Unification (
  UnificationError(..),
  UnifyConstraint(..),
  runSolve,
  unifyMany,
  unifies
) where

import           Control.Monad.Except
import           Control.Monad.Identity

import qualified Data.Map                as Map
import qualified Data.Set                as Set

import           Oden.Identifier
import           Oden.Infer.Substitution
import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data UnificationError = UnificationFail SourceInfo Type Type
                      | RowFieldUnificationFail SourceInfo (Identifier, Type) (Identifier, Type)
                      | InfiniteType SourceInfo TVar Type
                      | UnificationMismatch SourceInfo [Type] [Type]
                      deriving (Show, Eq)

data UnifyConstraint = UnifyConstraint SourceInfo Type Type

instance FTV UnifyConstraint where
  ftv (UnifyConstraint _ t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable UnifyConstraint where
  apply s (UnifyConstraint si t1 t2) =
    UnifyConstraint si (apply s t1) (apply s t2)

-- | UnifyConstraint solver monad.
type Solve a = ExceptT UnificationError Identity a

-- | Unifies the corresponding types in the lists (like a zip).
unifyMany :: SourceInfo -> [Type] -> [Type] -> Solve Subst
unifyMany _ [] [] = return emptySubst
unifyMany si (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies si t1 t2
     su2 <- unifyMany si (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany si t1 t2 = throwError $ UnificationMismatch si t1 t2

-- | Unify two types, returning the resulting substitution.
unifies :: SourceInfo -> Type -> Type -> Solve Subst
unifies si (TConstrained _ constrained) t = unifies si constrained t
unifies si t (TConstrained _ constrained) = unifies si t constrained
unifies _ (TVar _ v) t = v `bind` t
unifies _ t (TVar _ v) = v `bind` t
unifies _ (TCon _ n1) (TCon _ n2)
  | n1 == n2 = return emptySubst
unifies si (TFn _ t1 t2) (TFn _ t3 t4) = unifyMany si [t1, t2] [t3, t4]
unifies si (TNoArgFn _ t1) (TNoArgFn _ t2) = unifies si t1 t2
unifies si (TForeignFn _ v1 ps1 rs1) (TForeignFn _ v2 ps2 rs2) | v1 == v2 = do
  p <- unifyMany si ps1 ps2
  r <- unifyMany si rs1 rs2
  return (r `compose` p)
unifies si (TTuple _ f1 s1 r1) (TTuple _ f2 s2 r2) = do
  f <- unifies si f1 f2
  s <- unifies si s1 s2
  r <- unifyMany si r1 r2
  return (f `compose` s `compose` r)
unifies si (TSlice _ t1) (TSlice _ t2) = unifies si t1 t2
unifies si (TNamed _ n1 t1) (TNamed _ n2 t2)
  | n1 == n2 = unifies si t1 t2
unifies si t1 (TNamed _ _ t2) = unifies si t1 t2
unifies si (TNamed _ _ t1) t2 = unifies si t1 t2
unifies si (TRecord _ r1) (TRecord _ r2) =
  unifies si r1 r2
unifies _ REmpty{} REmpty{} = return emptySubst
unifies si r1@RExtension{} r2@RExtension{} = do
  -- Get all fields in the rows.
  let f1 = Map.fromList (rowToList r1)
      f2 = Map.fromList (rowToList r2)
      -- Get the unique fields in each row.
      onlyInR1 = f1 `Map.difference` f2
      onlyInR2 = f2 `Map.difference` f1

  -- Unify the common field with matching labels.
  substs <- sequence (Map.elems (Map.intersectionWith (unifies si) f1 f2))

  -- For both rows, unify the leaf row (possibly a row variable) with unique
  -- fields in the other row. In case both leafs are row variables, just unify
  -- those.
  leafSubst <- case (getLeafRow r1, getLeafRow r2) of
    (leaf1@TVar{}, leaf2@TVar{}) ->
      compose <$> unifies si leaf1 (rowFromList (Map.assocs onlyInR2) leaf2)
              <*> unifies si leaf2 (rowFromList (Map.assocs onlyInR1) leaf1)
    (leaf1, leaf2) ->
      compose <$> unifies si leaf1 (rowFromList (Map.assocs onlyInR2) (REmpty (Metadata si)))
              <*> unifies si leaf2 (rowFromList (Map.assocs onlyInR1) (REmpty (Metadata si)))

  -- Return all substitutions.
  return $ foldl1 compose (leafSubst : substs)

unifies si t1 t2 = throwError $ UnificationFail si t1 t2

-- Unification solver
solver :: Subst -> [UnifyConstraint] -> Solve Subst
solver su cs =
  case cs of
    [] -> return su
    (UnifyConstraint si t1 t2 : cs0) -> do
      su1  <- unifies si t1 t2
      solver (su1 `compose` su) (apply su1 cs0)

-- | Create a substitution from the 'TVar' to the 'Type', as long as the 'TVar'
-- does not occur in the 'Type'. In that case we have an infinite type, which
-- is an error.
bind ::  TVar -> Type -> Solve Subst
bind a (TVar _ v) | v == a = return emptySubst
bind a t
  | occursCheck a t = throwError $ InfiniteType (getSourceInfo t) a t
  | otherwise       = return (Subst $ Map.singleton a t)

-- | Check if the 'TVar' occurs in the 'Type'.
occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | Run the UnifyConstraint solver
runSolve :: [UnifyConstraint] -> Either UnificationError Subst
runSolve cs = runIdentity $ runExceptT $ solver emptySubst cs
