module Oden.Compiler.Instantiate (
  instantiate,
  InstantiateError(..)
) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map              as Map hiding (foldl, map)

import qualified Oden.Core             as Core
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

data InstantiateError = TypeMismatch Poly.Type Mono.Type
                      | SubstitutionFailed Poly.TVar [Poly.TVar]
                      deriving (Show, Eq, Ord)

type Substitutions = Map Poly.TVar Poly.Type
type Instantiate a = StateT Substitutions (Except InstantiateError) a

monoToPoly :: Mono.Type -> Poly.Type
monoToPoly Mono.TAny = Poly.TAny
monoToPoly (Mono.TCon n) = Poly.TCon n
monoToPoly (Mono.TNoArgFn f) = Poly.TNoArgFn (monoToPoly f)
monoToPoly (Mono.TFn f p) = Poly.TFn (monoToPoly f) (monoToPoly p)
monoToPoly (Mono.TUncurriedFn as r) = Poly.TUncurriedFn (map monoToPoly as) (monoToPoly r)
monoToPoly (Mono.TVariadicFn as v r) = Poly.TVariadicFn (map monoToPoly as) (monoToPoly v) (monoToPoly r)
monoToPoly (Mono.TSlice t) = Poly.TSlice (monoToPoly t)

getSubstitutions :: Poly.Type -> Mono.Type -> Either InstantiateError Substitutions
getSubstitutions p@(Poly.TCon pn) m@(Mono.TCon mn) =
  if pn == mn
  then Right Map.empty
  else Left (TypeMismatch p m)
getSubstitutions (Poly.TNoArgFn pf) (Mono.TNoArgFn mf) =
  getSubstitutions pf mf
getSubstitutions (Poly.TFn pf pp) (Mono.TFn mf mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `mappend` ps)
getSubstitutions (Poly.TVar v) mono = Right (Map.singleton v (monoToPoly mono))
getSubstitutions (Poly.TUncurriedFn pas pr) (Mono.TUncurriedFn mas mr) = do
  as <- zipWithM getSubstitutions pas mas
  r <- getSubstitutions pr mr
  return (foldl mappend r as)
getSubstitutions (Poly.TVariadicFn pas pv pr) (Mono.TVariadicFn mas mv mr) = do
  as <- zipWithM getSubstitutions pas mas
  r <- getSubstitutions pr mr
  v <- getSubstitutions pv mv
  return (foldl mappend r (v:as))
getSubstitutions (Poly.TSlice p) (Mono.TSlice m) =
  getSubstitutions p m
getSubstitutions poly mono = Left (TypeMismatch poly mono)

replace :: Poly.Type -> Instantiate Poly.Type
replace Poly.TAny = return Poly.TAny
replace (Poly.TVar v) = do
  s <- get
  case Map.lookup v s of
    Just mono -> return mono
    Nothing -> throwError (SubstitutionFailed v (Map.keys s))
replace (Poly.TCon n) = return (Poly.TCon n)
replace (Poly.TNoArgFn t) = Poly.TNoArgFn <$> replace t
replace (Poly.TFn ft pt) = Poly.TFn <$> replace ft <*> replace pt
replace (Poly.TUncurriedFn ft pt) =
  Poly.TUncurriedFn <$> mapM replace ft <*> replace pt
replace (Poly.TVariadicFn ft vt pt) =
  Poly.TVariadicFn <$> mapM replace ft <*> replace vt <*> replace pt
replace (Poly.TSlice t) = Poly.TSlice <$> replace t

instantiateExpr :: Core.Expr Poly.Type
                -> Instantiate (Core.Expr Poly.Type)
instantiateExpr (Core.Op o e1 e2 t) = Core.Op o <$> instantiateExpr e1 <*> instantiateExpr e2 <*> replace t
instantiateExpr (Core.Symbol i t) = Core.Symbol i <$> replace t
instantiateExpr (Core.Application f p t) =
  Core.Application <$> instantiateExpr f <*> instantiateExpr p <*> replace t
instantiateExpr (Core.NoArgApplication f t) =
  Core.NoArgApplication <$> instantiateExpr f <*> replace t
instantiateExpr (Core.UncurriedFnApplication f ps t) =
  Core.UncurriedFnApplication <$> instantiateExpr f <*> mapM instantiateExpr ps <*> replace t
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
instantiateExpr (Core.Slice es t) =
  Core.Slice <$> mapM instantiateExpr es <*> replace t

instantiate :: Core.Expr Poly.Type
            -> Mono.Type
            -> Either InstantiateError (Core.Expr Poly.Type)
instantiate expr mono = do
  subst <- getSubstitutions (Core.typeOf expr) mono
  (expr', _) <- runExcept (runStateT (instantiateExpr expr) subst)
  return expr'

