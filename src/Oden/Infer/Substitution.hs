{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Oden.Infer.Substitution where

import Oden.Type.Polymorphic
import Oden.Core as Core

import qualified Data.Set               as Set
import qualified Data.Map               as Map

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

class FTV a => Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply _ TAny                      = TAny
  apply _ (TCon a)                  = TCon a
  apply (Subst s) t@(TVar a)        = Map.findWithDefault t a s
  apply s (TNoArgFn t)            = TNoArgFn (apply s t)
  apply s (t1 `TFn` t2)            = apply s t1 `TFn` apply s t2
  apply s (TUncurriedFn as r)            = TUncurriedFn (map (apply s) as) (apply s r)
  apply s (TVariadicFn as v r)  = TVariadicFn (map (apply s) as) (apply s v) (apply s r)
  apply s (TSlice t)                = TSlice (apply s t)


instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as

instance FTV Core.CanonicalExpr where
  ftv (sc, expr) = ftv sc `Set.union` ftv expr

instance Substitutable Core.CanonicalExpr where
  apply s (sc, expr) = (apply s sc, apply s expr)

instance FTV (Core.Expr Type) where
  ftv = ftv . Core.typeOf

instance Substitutable (Core.Expr Type) where
  apply s (Core.Symbol x t)               = Core.Symbol x (apply s t)
  apply s (Core.Application f p t)        = Core.Application (apply s f) (apply s p) (apply s t)
  apply s (Core.NoArgApplication f t)     = Core.NoArgApplication (apply s f) (apply s t)
  apply s (Core.UncurriedFnApplication f p t)  = Core.UncurriedFnApplication (apply s f) (apply s p) (apply s t)
  apply s (Core.Fn x b t)                 = Core.Fn x (apply s b) (apply s t)
  apply s (Core.NoArgFn b t)              = Core.NoArgFn (apply s b) (apply s t)
  apply s (Core.Let x e b t)              = Core.Let x (apply s e) (apply s b) (apply s t)
  apply s (Core.Literal l t)              = Core.Literal l (apply s t)
  apply s (Core.If c tb fb t)             = Core.If (apply s c) (apply s tb) (apply s fb) (apply s t)
  apply s (Core.Slice es t)               = Core.Slice (apply s es) (apply s t)

instance Substitutable a => Substitutable [a] where
  apply = map . apply
