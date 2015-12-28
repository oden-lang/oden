module Oden.Compiler.Instantiate (
  instantiate,
  InstantiateError(..)
) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map              as Map

import qualified Oden.Core             as Core
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

data InstantiateError = TypeMismatch Poly.Type Mono.Type
                      | SubstitutionFailed Poly.TVar
                      deriving (Show, Eq, Ord)

type Substitutions = Map Poly.TVar Poly.Type
type Instantiate a = StateT Substitutions (Except InstantiateError) a

monoToPoly :: Mono.Type -> Poly.Type
monoToPoly (Mono.TCon n) = Poly.TCon n
monoToPoly (Mono.TArrSingle f) = Poly.TArrSingle (monoToPoly f)
monoToPoly (Mono.TArr f p) = Poly.TArr (monoToPoly f) (monoToPoly p)

getSubstitutions :: Poly.Type -> Mono.Type -> Either InstantiateError Substitutions
getSubstitutions p@(Poly.TCon pn) m@(Mono.TCon mn) =
  if pn == mn
  then Right Map.empty
  else Left (TypeMismatch p m)
getSubstitutions (Poly.TArrSingle pf) (Mono.TArrSingle mf) =
  getSubstitutions pf mf
getSubstitutions (Poly.TArr pf pp) (Mono.TArr mf mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `mappend` ps)
getSubstitutions (Poly.TVar v) mono = Right (Map.singleton v (monoToPoly mono))
getSubstitutions poly mono = Left (TypeMismatch poly mono)

replace :: Poly.Type -> Instantiate Poly.Type
replace (Poly.TVar v) = do
  s <- get
  case Map.lookup v s of
    Just mono -> return mono
    Nothing -> throwError (SubstitutionFailed v)
replace (Poly.TCon n) = return (Poly.TCon n)
replace (Poly.TArrSingle t) = Poly.TArrSingle <$> replace t
replace (Poly.TArr ft pt) = Poly.TArr <$> replace ft <*> replace pt

instantiateExpr :: Core.Expr Poly.Type
                -> Instantiate (Core.Expr Poly.Type)
instantiateExpr (Core.Symbol i t) = Core.Symbol i <$> replace t
instantiateExpr (Core.Application f p t) =
  Core.Application <$> instantiateExpr f <*> instantiateExpr p <*> replace t
instantiateExpr (Core.NoArgApplication f t) =
  Core.NoArgApplication <$> instantiateExpr f <*> replace t
instantiateExpr (Core.Fn a b t) =
  Core.Fn a <$> instantiateExpr b <*> replace t
instantiateExpr (Core.NoArgFn b t) =
  Core.NoArgFn <$> instantiateExpr b <*> replace t
instantiateExpr (Core.Let name e b t) =
  Core.Let name <$> instantiateExpr e <*> instantiateExpr b <*> replace t
instantiateExpr (Core.Literal l t) =
  Core.Literal l <$> replace t
instantiateExpr (Core.If c tb eb t) =
  Core.If <$> instantiateExpr c <*> instantiateExpr tb <*> instantiateExpr eb <*> replace t
instantiateExpr (Core.Fix e t) =
  Core.Fix <$> instantiateExpr e <*> replace t

instantiate :: Core.Expr Poly.Type
            -> Mono.Type
            -> Either InstantiateError (Core.Expr Poly.Type)
instantiate expr mono = do
  subst <- getSubstitutions (Core.typeOf expr) mono
  (expr', _) <- runExcept (runStateT (instantiateExpr expr) subst)
  return expr'

