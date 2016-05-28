{-# LANGUAGE LambdaCase #-}
-- | Instantiates polymorphically typed expressions with monomorphic types.
module Oden.Compiler.Instantiate (
  instantiate,
  InstantiateError(..)
) where

import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Map              as Map hiding (foldl)

import           Oden.Core.Expr
import           Oden.Core.Typed
import           Oden.Metadata
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

data InstantiateError = TypeMismatch SourceInfo Poly.Type Mono.Type
                      | SubstitutionFailed SourceInfo Poly.TVar [Poly.TVar]
                      deriving (Show, Eq, Ord)

type Substitutions = Map.Map Poly.TVar Poly.Type
type Instantiate a = StateT Substitutions (Except InstantiateError) a

monoToPoly :: Mono.Type -> Poly.Type
monoToPoly (Mono.TTuple si f s r) =
  Poly.TTuple si (monoToPoly f) (monoToPoly s) (map monoToPoly r)
monoToPoly (Mono.TCon si n) = Poly.TCon si n
monoToPoly (Mono.TApp si f p) = Poly.TApp si (monoToPoly f) (monoToPoly p)
monoToPoly (Mono.TNoArgFn si f) = Poly.TNoArgFn si (monoToPoly f)
monoToPoly (Mono.TFn si f p) = Poly.TFn si (monoToPoly f) (monoToPoly p)
monoToPoly (Mono.TForeignFn si variadic as r) =
  Poly.TForeignFn si variadic (map monoToPoly as) (map monoToPoly r)
monoToPoly (Mono.TSlice si t) = Poly.TSlice si (monoToPoly t)
monoToPoly (Mono.TRecord si row) = Poly.TRecord si (monoToPoly row)
monoToPoly (Mono.TNamed si n t) = Poly.TNamed si n (monoToPoly t)
monoToPoly (Mono.REmpty si) = Poly.REmpty si
monoToPoly (Mono.RExtension si label polyType row) =
  Poly.RExtension si label (monoToPoly polyType) (monoToPoly row)

getSubstitutions :: Poly.Type -> Mono.Type -> Either InstantiateError Substitutions
getSubstitutions (Poly.TCon _ pn) (Mono.TCon _ mn)
  | pn == mn = Right Map.empty
getSubstitutions (Poly.TNoArgFn _ pf) (Mono.TNoArgFn _ mf) =
  getSubstitutions pf mf
getSubstitutions (Poly.TFn _ pf pp) (Mono.TFn _ mf mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `mappend` ps)
getSubstitutions (Poly.TVar _ v) mono = Right (Map.singleton v (monoToPoly mono))
getSubstitutions (Poly.TForeignFn _ _ pas prs) (Mono.TForeignFn _ _ mas mrs) = do
  as <- zipWithM getSubstitutions pas mas
  rs <- zipWithM getSubstitutions prs mrs
  return (mconcat (as ++ rs))
getSubstitutions (Poly.TTuple _ pf ps pr) (Mono.TTuple _ mf ms mr) = do
  f <- getSubstitutions pf mf
  s <- getSubstitutions ps ms
  r <- zipWithM getSubstitutions pr mr
  return (foldl mappend f (s:r))
getSubstitutions (Poly.TSlice _ p) (Mono.TSlice _ m) =
  getSubstitutions p m

getSubstitutions (Poly.TRecord _ polyRow) (Mono.TRecord _ monoRow) =
  getSubstitutions polyRow monoRow

getSubstitutions polyRow@(Poly.RExtension (Metadata _psi) _ _ _) monoRow@(Mono.RExtension (Metadata msi) _ _ _) = do

  -- Extract the fields in each rows.
  let polyFields = Map.fromList (Poly.rowToList polyRow)
      monoFields = Map.fromList (Mono.rowToList monoRow)
      onlyInPoly = polyFields `Map.difference` monoFields
      onlyInMono = monoFields `Map.difference` polyFields

  unless (Map.null onlyInPoly) $
    throwError (TypeMismatch msi polyRow monoRow)

  -- Get substitutions the common fields.
  substs <- sequence (Map.elems (Map.intersectionWith getSubstitutions polyFields monoFields))

  -- Retrieve leaf rows.
  let polyLeaf = Poly.getLeafRow polyRow

  -- Only the polymorphic leaf (possible a row variable) with the remaining
  -- monomorphic fields.
  leafSubst <- getSubstitutions polyLeaf (Mono.rowFromList $ Map.assocs onlyInMono)

  return (foldl mappend leafSubst substs)

getSubstitutions Poly.REmpty{} Mono.REmpty{} = return Map.empty
getSubstitutions poly mono = throwError (TypeMismatch (getSourceInfo mono) poly mono)

replace :: Poly.Type -> Instantiate Poly.Type
replace (Poly.TTuple si f s r) =
  Poly.TTuple si <$> replace f <*> replace s <*> mapM replace r
replace (Poly.TVar (Metadata si) v) = do
  s <- get
  case Map.lookup v s of
    Just mono -> return (setSourceInfo si mono)
    Nothing -> throwError (SubstitutionFailed si v (Map.keys s))
replace (Poly.TCon si n) = return (Poly.TCon si n)
replace (Poly.TApp si ft pt) = Poly.TApp si <$> replace ft <*> replace pt
replace (Poly.TNoArgFn si t) = Poly.TNoArgFn si <$> replace t
replace (Poly.TFn si ft pt) = Poly.TFn si <$> replace ft <*> replace pt
replace (Poly.TForeignFn si variadic ft pt) =
  Poly.TForeignFn si variadic <$> mapM replace ft <*> mapM replace pt
replace (Poly.TSlice si t) = Poly.TSlice si <$> replace t
replace (Poly.TRecord si r) = Poly.TRecord si <$> replace r
replace (Poly.TNamed si n t) = Poly.TNamed si n <$> replace t
replace (Poly.TConstrained constraints t) = Poly.TConstrained constraints <$> replace t
replace (Poly.REmpty si) = return $ Poly.REmpty si
replace (Poly.RExtension si label type' row) =
  Poly.RExtension si label <$> replace type'
                           <*> replace row

instantiateField :: FieldInitializer TypedExpr
                -> Instantiate (FieldInitializer TypedExpr)
instantiateField (FieldInitializer si label expr) =
  FieldInitializer si label <$> instantiateExpr expr

instantiateMemberAccess :: TypedMemberAccess
                        -> Instantiate TypedMemberAccess
instantiateMemberAccess = \case
  RecordFieldAccess expr name ->
    RecordFieldAccess <$> instantiateExpr expr <*> return name
  c@PackageMemberAccess{} -> return c

instantiateExpr :: TypedExpr
                -> Instantiate TypedExpr
instantiateExpr (Symbol si i t) =
  Symbol si i <$> replace t
instantiateExpr (Subscript si s i t) =
  Subscript si <$> instantiateExpr s
                    <*> instantiateExpr i
                    <*> replace t

instantiateExpr (Subslice si s (Range e1 e2) t) =
  Subslice si <$> instantiateExpr s
                   <*> (Range <$> instantiateExpr e1 <*> instantiateExpr e2)
                   <*> replace t
instantiateExpr (Subslice si s (RangeTo e) t) =
  Subslice si <$> instantiateExpr s
                   <*> (RangeTo <$> instantiateExpr e)
                   <*> replace t
instantiateExpr (Subslice si s (RangeFrom e) t) =
  Subslice si <$> instantiateExpr s
                   <*> (RangeFrom <$> instantiateExpr e)
                   <*> replace t

instantiateExpr (Application si f p t) =
  Application si <$> instantiateExpr f
                      <*> instantiateExpr p
                      <*> replace t
instantiateExpr (NoArgApplication si f t) =
  NoArgApplication si <$> instantiateExpr f
                           <*> replace t
instantiateExpr (ForeignFnApplication si f ps t) =
  ForeignFnApplication si <$> instantiateExpr f
                               <*> mapM instantiateExpr ps
                               <*> replace t
instantiateExpr (Fn si a b t) =
  Fn si a <$> instantiateExpr b
               <*> replace t
instantiateExpr (NoArgFn si b t) =
  NoArgFn si <$> instantiateExpr b
                  <*> replace t
instantiateExpr (Let si name e b t) =
  Let si name <$> instantiateExpr e
                   <*> instantiateExpr b
                   <*> replace t
instantiateExpr (Literal si l t) =
  Literal si l <$> replace t
instantiateExpr (If si c tb eb t) =
  If si <$> instantiateExpr c
             <*> instantiateExpr tb
             <*> instantiateExpr eb
             <*> replace t
instantiateExpr (Tuple si fe se rs t) =
  Tuple si <$> instantiateExpr fe
                <*> instantiateExpr se
                <*> mapM instantiateExpr rs
                <*> replace t
instantiateExpr (Slice si es t) =
  Slice si <$> mapM instantiateExpr es
                <*> replace t
instantiateExpr (Block si es t) =
  Block si <$> mapM instantiateExpr es
                <*> replace t
instantiateExpr (RecordInitializer si fields t) =
  RecordInitializer si <$> mapM instantiateField fields <*> replace t
instantiateExpr (MemberAccess si access t) =
  MemberAccess si <$> instantiateMemberAccess access <*> replace t
instantiateExpr (MethodReference si ref t) =
  MethodReference si ref <$> replace t
instantiateExpr (Foreign si f t) =
  Foreign si f <$> replace t

-- | Given a polymorphically typed expression and a monomorphic type, return
-- the expression with all types substitued for monomorphic ones. If there's
-- a mismatch an error is thrown.
instantiate :: TypedExpr
            -> Mono.Type
            -> Either InstantiateError TypedExpr
instantiate expr mono = do
  subst <- getSubstitutions (typeOf expr) mono
  (expr', _) <- runExcept (runStateT (instantiateExpr expr) subst)
  return expr'
