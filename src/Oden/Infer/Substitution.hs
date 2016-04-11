{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Oden.Infer.Substitution where

import           Oden.Core             as Core
import           Oden.Core.Expr
import           Oden.Type.Polymorphic as Poly

import qualified Data.Map              as Map
import qualified Data.Set              as Set

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Union of substitutions
union :: Subst -> Subst -> Subst
(Subst s1) `union` (Subst s2) = Subst (s1 `Map.union` s2)

class FTV a => Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance (Substitutable a, Ord a) => Substitutable (Set.Set a) where
  apply = Set.map . apply

instance Substitutable Type where
  apply s (TTuple si f s' r)            = TTuple si (apply s f) (apply s s') (apply s r)
  apply _ (TCon si n)                   = TCon si n
  apply (Subst s) t@(TVar _ a)          = Map.findWithDefault t a s
  apply s (TNoArgFn si t)               = TNoArgFn si (apply s t)
  apply s (TFn si t1 t2)                = TFn si (apply s t1) (apply s t2)
  apply s (TForeignFn si variadic as r) = TForeignFn si variadic (map (apply s) as) (apply s r)
  apply s (TSlice si t)                 = TSlice si (apply s t)
  apply s (TRecord si r)                = TRecord si (apply s r)
  apply _ (REmpty si)                   = REmpty si
  apply s (RExtension si l t r)         = uniqueRow (RExtension si l (apply s t) (apply s r))
  apply s (TNamed si n t)               = TNamed si n (apply s t)
  apply s (TConstrained cs t)           = TConstrained (apply s cs) (apply s t)

instance Substitutable ProtocolConstraint where
  apply s (ProtocolConstraint si protocol type') =
    ProtocolConstraint si (apply s protocol) (apply s type')

instance Substitutable Scheme where
  apply (Subst s) (Forall si qs cs t) =
    Forall si qs (apply s' cs) (apply s' t)
    where s' = Subst $ foldr (Map.delete . getBindingVar) s qs

instance FTV UnresolvedMethodReference where
  ftv (UnresolvedMethodReference protocol method) =
    ftv protocol `Set.union` ftv method

instance Substitutable UnresolvedMethodReference where
  apply s (UnresolvedMethodReference protocol method) =
    UnresolvedMethodReference (apply s protocol) (apply s method)

instance FTV CanonicalExpr where
  ftv (sc, expr) = ftv sc `Set.union` ftv expr

instance Substitutable CanonicalExpr where
  apply s (sc, expr) = (apply s sc, apply s expr)

instance FTV (FieldInitializer r Type) where
  ftv (FieldInitializer _ _ expr) = ftv expr

instance Substitutable r => Substitutable (FieldInitializer r Type) where
  apply s (FieldInitializer si label expr) =
    FieldInitializer si label (apply s expr)

instance FTV (Expr r Type) where
  ftv = ftv . typeOf

instance (Substitutable r) => Substitutable (Expr r Type) where
  apply s (Symbol si x t)                                = Symbol si x (apply s t)
  apply s (Subscript si es i t)                          = Subscript si (apply s es) (apply s i) (apply s t)
  apply s (Subslice si es (Range e1 e2) t)               = Subslice si (apply s es) (Range (apply s e1) (apply s e2)) (apply s t)
  apply s (Subslice si es (RangeTo e) t)                 = Subslice si (apply s es) (RangeTo (apply s e)) (apply s t)
  apply s (Subslice si es (RangeFrom e) t)               = Subslice si (apply s es) (RangeFrom (apply s e)) (apply s t)
  apply s (UnaryOp si o e t)                             = UnaryOp si o (apply s e) (apply s t)
  apply s (BinaryOp si o e1 e2 t)                        = BinaryOp si o (apply s e1) (apply s e2) (apply s t)
  apply s (Application si f p t)                         = Application si (apply s f) (apply s p) (apply s t)
  apply s (NoArgApplication si f t)                      = NoArgApplication si (apply s f) (apply s t)
  apply s (ForeignFnApplication si f p t)                = ForeignFnApplication si (apply s f) (apply s p) (apply s t)
  apply s (Fn si x b t)                                  = Fn si x (apply s b) (apply s t)
  apply s (NoArgFn si b t)                               = NoArgFn si (apply s b) (apply s t)
  apply s (Let si x e b t)                               = Let si x (apply s e) (apply s b) (apply s t)
  apply s (Literal si l t)                               = Literal si l (apply s t)
  apply s (Tuple si fe se re t)                          = Tuple si (apply s fe) (apply s se) (apply s re) (apply s t)
  apply s (If si c tb fb t)                              = If si (apply s c) (apply s tb) (apply s fb) (apply s t)
  apply s (Slice si es t)                                = Slice si (apply s es) (apply s t)
  apply s (Block si es t)                                = Block si (apply s es) (apply s t)
  apply s (RecordInitializer si t fs)                    = RecordInitializer si (apply s t) (apply s fs)
  apply s (RecordFieldAccess si expr name t)             = RecordFieldAccess si (apply s expr) name (apply s t)
  apply s (PackageMemberAccess si pkgAlias name t)       = PackageMemberAccess si pkgAlias name (apply s t)
  apply s (MethodReference si ref t)                     = MethodReference si (apply s ref) (apply s t)

instance Substitutable ProtocolMethod where
  apply s (ProtocolMethod si name scheme) =
    ProtocolMethod si name (apply s scheme)

instance Substitutable Protocol where
  apply s (Protocol si name var methods) =
    Protocol si name (apply s var) (apply s methods)
