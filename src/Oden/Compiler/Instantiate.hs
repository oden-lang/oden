{-# LANGUAGE LambdaCase #-}
-- | Instantiates polymorphically typed expressions with monomorphic types.
module Oden.Compiler.Instantiate (
  instantiate,
  InstantiateError(..)
) where

import           Control.Monad.Except

import qualified Data.Map              as Map hiding (foldl)

import           Oden.Core.Expr
import           Oden.Core.Typed
import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Substitution as Substitution
import           Oden.Type.Polymorphic

data InstantiateError = TypeMismatch SourceInfo Type Type
                      | SubstitutionFailed SourceInfo TVar [TVar]
                      deriving (Show, Eq, Ord)

getSubstitutions :: Type -> Type -> Either InstantiateError Subst
getSubstitutions (TConstrained _ x) y = getSubstitutions x y
getSubstitutions x (TConstrained _ y) = getSubstitutions x y
getSubstitutions (TCon _ pn) (TCon _ mn)
  | pn == mn = Right emptySubst
getSubstitutions (TNoArgFn _ pf) (TNoArgFn _ mf) =
  getSubstitutions pf mf
getSubstitutions (TFn _ pf pp) (TFn _ mf mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `mappend` ps)
getSubstitutions (TVar _ v) mono = Right (Substitution.singleton v mono)
getSubstitutions (TForeignFn _ _ pas prs) (TForeignFn _ _ mas mrs) = do
  as <- zipWithM getSubstitutions pas mas
  rs <- zipWithM getSubstitutions prs mrs
  return (mconcat (as ++ rs))
getSubstitutions (TTuple _ pf ps pr) (TTuple _ mf ms mr) = do
  f <- getSubstitutions pf mf
  s <- getSubstitutions ps ms
  r <- zipWithM getSubstitutions pr mr
  return (foldl compose f (s:r))
getSubstitutions (TSlice _ p) (TSlice _ m) =
  getSubstitutions p m

getSubstitutions (TRecord _ polyRow) (TRecord _ monoRow) =
  getSubstitutions polyRow monoRow

getSubstitutions polyRow@(RExtension (Metadata _psi) _ _ _) monoRow@(RExtension (Metadata msi) _ _ _) = do

  -- Extract the fields in each rows.
  let polyFields = Map.fromList (rowToList polyRow)
      monoFields = Map.fromList (rowToList monoRow)
      onlyInPoly = polyFields `Map.difference` monoFields
      onlyInMono = monoFields `Map.difference` polyFields

  unless (Map.null onlyInPoly) $
    throwError (TypeMismatch msi polyRow monoRow)

  -- Get substitutions the common fields.
  substs <- sequence (Map.elems (Map.intersectionWith getSubstitutions polyFields monoFields))

  -- Retrieve leaf rows.
  let polyLeaf = getLeafRow polyRow

  -- Only the polymorphic leaf (possible a row variable) with the remaining
  -- monomorphic fields.
  leafSubst <- getSubstitutions polyLeaf (rowFromList  (Map.assocs onlyInMono) (REmpty (Metadata Missing)))

  return (foldl mappend leafSubst substs)

getSubstitutions REmpty{} REmpty{} = return emptySubst
getSubstitutions poly mono = throwError (TypeMismatch (getSourceInfo mono) poly mono)


-- | Given a polymorphically typed expression and a monomorphic type, return
-- the expression with all types substitued for monomorphic ones. If there's
-- a mismatch an error is thrown.
instantiate :: TypedExpr
            -> Type
            -> Either InstantiateError TypedExpr
instantiate expr instanceType = do
  subst <- getSubstitutions (typeOf expr) instanceType
  return (subst `apply` expr)
