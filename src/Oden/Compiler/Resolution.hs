{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Resolves protocol method usage to implementation if there's a single
-- possible resolution that matches. Note that we only make local protocol
-- implementations available for now.
module Oden.Compiler.Resolution where

import Oden.Core.Expr
import Oden.Core.Typed
import Oden.Core.Definition
import Oden.Core.ProtocolImplementation
import Oden.Core.Traversal

import Oden.Infer.Subsumption
import Oden.Infer.Substitution
import Oden.Infer.Unification

import Oden.Metadata
import Oden.SourceInfo
import Oden.Type.Polymorphic

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Set as Set
import           Data.Set (Set)

data ResolutionError
  = NoMatchingImplementationInScope SourceInfo Protocol ProtocolMethod [ProtocolImplementation TypedExpr]
  | MultipleMatchingImplementationsInScope SourceInfo [ProtocolImplementation TypedExpr]
  deriving (Show, Eq, Ord)

type ResolutionEnvironment = Set (ProtocolImplementation TypedExpr)

type Resolve = StateT ResolutionEnvironment (Except ResolutionError)

matching :: Protocol
         -> ProtocolMethod
         -> Type
         -> ProtocolImplementation TypedExpr
         -> [(ProtocolImplementation TypedExpr, MethodImplementation TypedExpr)]
matching protocol method type' impl =
  case impl of
    ProtocolImplementation _ (Protocol _ implProtocolName _ _) _ _
      | implProtocolName /= protocolName protocol -> []
    ProtocolImplementation _ _ _ methodImpls -> do
      methodImpl <- concatMap matchingMethod methodImpls
      return (impl, methodImpl)
  where
  matchingMethod methodImpl =
    let (ProtocolMethod _ methodName _) = method
        (MethodImplementation _ (ProtocolMethod _ implMethodName _) expr) = methodImpl in
    case runSolve [UnifyConstraint (getSourceInfo type') (typeOf expr) type'] of
      Left _ -> []
      Right subst
        | implMethodName == methodName -> [apply subst methodImpl]
        | otherwise                    -> []

instantiateImplementation :: MethodImplementation TypedExpr
                          -> Type
                          -> Resolve (MethodImplementation TypedExpr)
instantiateImplementation (MethodImplementation si method expr) type' =
  case type' `typeSubsumedBy` typeOf expr of
    Left err -> error (show err)
    Right subst -> return (MethodImplementation si method (apply subst expr))

-- | Super-primitive protocol implementation lookup for now. It should check
-- what methods implementations unify.
lookupMethodImplementation :: SourceInfo
                           -> Protocol
                           -> ProtocolMethod
                           -> Type
                           -> Resolve (MethodImplementation TypedExpr)
lookupMethodImplementation si protocol method type' = do
  impls <- gets Set.toList
  methodImpl <- findProtocolMethodImpl impls
  instantiateImplementation methodImpl type'
  where
  findProtocolMethodImpl allImpls =
    case concatMap (matching protocol method type') allImpls of
      []     -> throwError (NoMatchingImplementationInScope si protocol method allImpls)
      [(_, methodImpl)] -> return methodImpl
      impls  -> throwError (MultipleMatchingImplementationsInScope si (map fst impls))

resolveInMethodImplementation :: MethodImplementation TypedExpr
                              -> Resolve (MethodImplementation TypedExpr)
resolveInMethodImplementation (MethodImplementation si method expr) =
  MethodImplementation si method <$> resolveInExpr' expr

resolveInImplementation :: ProtocolImplementation TypedExpr
                        -> Resolve (ProtocolImplementation TypedExpr)
resolveInImplementation (ProtocolImplementation si protocol implHead methods) =
  ProtocolImplementation si protocol implHead <$> mapM resolveInMethodImplementation methods

resolveInExpr' :: TypedExpr -> Resolve TypedExpr
resolveInExpr' = traverseExpr traversal
  where
  traversal = Traversal { onExpr = const (return Nothing)
                        , onType = return
                        , onMemberAccess = onMemberAccess'
                        , onNameBinding = return
                        , onMethodReference = onMethodReference' }
  onMethodReference' si reference type' =
    case reference of
      Unresolved protocol method -> do
        implementationMethod <- lookupMethodImplementation (unwrap si) protocol method type'
        return (si, Resolved protocol method implementationMethod, type')
      Resolved{} ->
        return (si, reference, type')
  onMemberAccess' = \case
    RecordFieldAccess expr identifier ->
      RecordFieldAccess <$> resolveInExpr' expr <*> return identifier
    PackageMemberAccess pkg member ->
      return (PackageMemberAccess pkg member)

resolveInDefinition' :: TypedDefinition -> Resolve TypedDefinition
resolveInDefinition' = \case
    Definition si name (scheme, expr) -> do
      expr' <- resolveInExpr' expr
      return (Definition si name (scheme, expr'))
    ForeignDefinition si name scheme ->
      return (ForeignDefinition si name scheme)
    TypeDefinition si name bindings type' ->
      return (TypeDefinition si name bindings type')
    ProtocolDefinition si name protocol ->
      return (ProtocolDefinition si name protocol)
    Implementation si implementation -> do
      resolved <- resolveInImplementation implementation
      modify (Set.insert resolved)
      return (Implementation si resolved)

runResolve :: ResolutionEnvironment -> Resolve a -> Either ResolutionError a
runResolve env = runExcept . flip evalStateT env

resolveInExpr :: ResolutionEnvironment
              -> TypedExpr
              -> Either ResolutionError TypedExpr
resolveInExpr env expr = runResolve env (resolveInExpr' expr)

resolveInDefinition :: ResolutionEnvironment
                    -> TypedDefinition
                    -> Either ResolutionError TypedDefinition
resolveInDefinition env def = runResolve env (resolveInDefinition' def)

resolveInDefinitions :: ResolutionEnvironment
                     -> [TypedDefinition]
                     -> Either ResolutionError [TypedDefinition]
resolveInDefinitions env defs = runResolve env (mapM resolveInDefinition' defs)

resolveInPackage :: ResolutionEnvironment
                 -> TypedPackage
                 -> Either ResolutionError TypedPackage
resolveInPackage env (TypedPackage decl imports defs) =
  TypedPackage decl imports <$> resolveInDefinitions env defs
