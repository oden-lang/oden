module Oden.Infer.Subsumption (
  Subsuming,
  SubsumptionError(..),
  subsume,
  subsumeTypeSignature,
  getSubst
) where

import Oden.Type.Polymorphic
import Oden.Core as Core
import Oden.Infer.Substitution

import           Control.Monad
import qualified Data.Map               as Map

data SubsumptionError = SubsumptionError Type Type
                      deriving (Show, Eq)

class Subsuming s where
  subsume :: s -> s -> Either SubsumptionError s

getSubst :: Type -> Type -> Either SubsumptionError Subst
getSubst TUnit{} TUnit{} = return emptySubst
getSubst (TBasic _ b1) (TBasic _ b2)
  | b1 == b2 = return emptySubst
getSubst t (TVar _ tv) = return (Subst (Map.singleton tv t))
getSubst (TFn _ a1 r1) (TFn _ a2 r2) = do
  a <- getSubst a1 a2
  r <- getSubst r1 r2
  return (a `union` r)
getSubst (TNoArgFn _ r1) (TNoArgFn _ r2) = getSubst r1 r2
getSubst (TUncurriedFn _ a1 r1) (TUncurriedFn _ a2 r2) = do
  as <- mapM (uncurry getSubst) ((r1, r2) : zip a1 a2)
  return (foldl union emptySubst as)
getSubst (TVariadicFn _ a1 v1 r1) (TVariadicFn _ a2 v2 r2) = do
  as <- mapM (uncurry getSubst) ((r1, r2) : (v1, v2) : zip a1 a2)
  return (foldl union emptySubst as)
getSubst (TTuple _ f1 s1 r1) (TTuple _ f2 s2 r2) = do
  f <- getSubst f1 f2
  s <- getSubst s1 s2
  r <- zipWithM getSubst r1 r2
  return (foldl union (f `union` s) r)
getSubst TAny{} _ = return emptySubst
getSubst t1 t2
  | t1 == t2  = return emptySubst
  | otherwise = Left (SubsumptionError t1 t2)

subsumeTypeSignature :: Scheme -> Core.Expr Type -> Either SubsumptionError Core.CanonicalExpr
subsumeTypeSignature s@(Forall _ _ st) expr = do
  subst <- getSubst st (Core.typeOf expr)
  return (s, apply subst expr)

instance Subsuming Type where
  t1@TAny{} `subsume` TAny{} = Right t1
  t1 `subsume` t2@TAny{} = Left (SubsumptionError t1 t2)
  t1@TAny{} `subsume` _ = Right t1
  t1@(TVar _ v1) `subsume` (TVar _ v2)
    | v1 == v2 = Right t1
  t1@(TBasic _ b1) `subsume` (TBasic _ b2)
    | b1 == b2 = return t1
  t1@TUnit{} `subsume` TUnit{} = return t1
  t1@(TTuple _ f1 s1 r1) `subsume` (TTuple _ f2 s2 r2) = do
    _ <- f1 `subsume` f2
    _ <- s1 `subsume` s2
    mapM_ (uncurry subsume) (zip r1 r2)
    return t1
  t1@(TCon _ s1) `subsume` (TCon _ s2)
    | s1 == s2 = return t1
  t1@(TNoArgFn _ at1) `subsume` (TNoArgFn _ at2) = do
    _ <- at1 `subsume` at2
    return t1
  t1@(TFn _ at1 rt1) `subsume` (TFn _ at2 rt2) = do
    _ <- at1 `subsume` at2
    _ <- rt1 `subsume` rt2
    return t1
  t1@(TSlice _ st1) `subsume` (TSlice _ st2) = do
    _ <- st1 `subsume` st2
    return t1
  t1@(TUncurriedFn _ ats1 rt1) `subsume` (TUncurriedFn _ ats2 rt2) = do
    mapM_ (uncurry subsume) (zip ats1 ats2)
    _ <- rt1 `subsume` rt2
    return t1
  t1@(TVariadicFn _ ats1 vt1 rt1) `subsume` (TVariadicFn _ ats2 vt2 rt2) = do
    mapM_ (uncurry subsume) (zip ats1 ats2)
    _ <- vt1 `subsume` vt2
    _ <- rt1 `subsume` rt2
    return t1
  t1 `subsume` t2 = Left (SubsumptionError t1 t2)
