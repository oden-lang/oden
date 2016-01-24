module Oden.Infer.Subsumption (
  Subsuming,
  SubsumptionError(..),
  subsume,
  subsumeTypeSignature
) where

import Oden.Type.Polymorphic
import Oden.Core as Core
import Oden.Infer.Substitution

import qualified Data.Map               as Map

data SubsumptionError = SubsumptionError Type Type
                      deriving (Show, Eq)

class Subsuming s where
  subsume :: s -> s -> Either SubsumptionError s

subsumeTypeSignature :: Scheme -> Core.Expr Type -> Either SubsumptionError Core.CanonicalExpr
subsumeTypeSignature s@(Forall _ st) expr = do
  subst <- getSubst st (Core.typeOf expr)
  return (s, apply subst expr)
  where
  getSubst :: Type -> Type -> Either SubsumptionError Subst
  getSubst t (TVar tv) = return (Subst (Map.singleton tv t))
  getSubst (TFn a1 r1) (TFn a2 r2) = do
    a <- getSubst a1 a2
    r <- getSubst r1 r2
    return (a `compose` r)
  getSubst (TNoArgFn r1) (TNoArgFn r2) = getSubst r1 r2
  getSubst (TUncurriedFn a1 r1) (TUncurriedFn a2 r2) = do
    as <- mapM (uncurry getSubst) ((r1, r2) : zip a1 a2)
    return (foldl compose emptySubst as)
  getSubst (TVariadicFn a1 v1 r1) (TVariadicFn a2 v2 r2) = do
    as <- mapM (uncurry getSubst) ((r1, r2) : (v1, v2) : zip a1 a2)
    return (foldl compose emptySubst as)
  getSubst TAny _ = return emptySubst
  getSubst t1 t2
    | t1 == t2  = return emptySubst
    | otherwise = Left (SubsumptionError t1 t2)

instance Subsuming Type where
  TAny `subsume` TAny = Right TAny
  t `subsume` TAny = Left (SubsumptionError t TAny)
  TAny `subsume` _ = Right TAny
  t1@(TNoArgFn at1) `subsume` (TNoArgFn at2) = do
    _ <- at1 `subsume` at2
    return t1
  t1@(TFn at1 rt1) `subsume` (TFn at2 rt2) = do
    _ <- at1 `subsume` at2
    _ <- rt1 `subsume` rt2
    return t1
  t1@(TUncurriedFn ats1 rt1) `subsume` (TUncurriedFn ats2 rt2) = do
    mapM_ (uncurry subsume) (zip ats1 ats2)
    _ <- rt1 `subsume` rt2
    return t1
  t1@(TVariadicFn ats1 vt1 rt1) `subsume` (TVariadicFn ats2 vt2 rt2) = do
    mapM_ (uncurry subsume) (zip ats1 ats2)
    _ <- vt1 `subsume` vt2
    _ <- rt1 `subsume` rt2
    return t1
  t1@(TSlice st1) `subsume` (TSlice st2) = do
    _ <- st1 `subsume` st2
    return t1
  t1 `subsume` t2
    | t1 == t2 = Right t1
    | otherwise = Left (SubsumptionError t1 t2)

