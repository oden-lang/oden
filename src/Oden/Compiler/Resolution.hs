{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Resolves protocol method usage to implementation if there's a single
-- possible resolution that matches. Note that we only make local protocol
-- implementations available for now.
module Oden.Compiler.Resolution where

import Oden.Core as Core
import Oden.Core.Definition
import Oden.Core.Expr
import Oden.Core.ProtocolImplementation
import Oden.Core.Resolved as Resolved
import Oden.Core.Traversal
import Oden.Metadata
import Oden.SourceInfo
import Oden.Type.Polymorphic

import Control.Monad.Except
import Control.Monad.State

data ResolutionError
  = NoMatchingImplementationInScope SourceInfo
  | MultipleMatchingImplementationsInScope SourceInfo [ProtocolImplementation TypedExpr]
  deriving (Show, Eq, Ord)

type ResolutionEnvironment = [ProtocolImplementation TypedExpr]

type Resolve = StateT ResolutionEnvironment (Except ResolutionError)

-- | Super-primitive protocol implementation lookup for now. It should check
-- what methods implementations unify.
lookupMethodImplementation :: SourceInfo
                           -> Protocol
                           -> ProtocolMethod
                           -> Resolve (MethodImplementation TypedExpr)
lookupMethodImplementation si protocol method = do
  matchingImpls <- filter matchesProtocol <$> get
  protocolImpl <- selectSingleProtocolImpl matchingImpls
  selectSingleMethod protocolImpl
  where
  matchesProtocol (ProtocolImplementation _ implProtocol _) =
    protocol == implProtocol
  selectSingleProtocolImpl = \case
    []     -> throwError (NoMatchingImplementationInScope si)
    [impl] -> return impl
    impls  -> throwError (MultipleMatchingImplementationsInScope si impls)
  matchesMethod (MethodImplementation _ implMethod _) =
    method == implMethod
  selectSingleMethod (ProtocolImplementation _ _ methodImpls) =
    case filter matchesMethod methodImpls of
      [] -> error "OMG no method impl"
      [methodImpl] -> return methodImpl
      _methodImpls -> error "OMG too many method impls"

resolveInMethodImplementation :: MethodImplementation TypedExpr
                              -> Resolve (MethodImplementation ResolvedExpr)
resolveInMethodImplementation (MethodImplementation si method expr) =
  MethodImplementation si method <$> resolveInExpr' expr

resolveInExpr' :: TypedExpr -> Resolve ResolvedExpr
resolveInExpr' = traverseExpr traversal
  where
  traversal = Traversal { onExpr = const (return Nothing)
                        , onType = return
                        , onMemberAccess = onMemberAccess'
                        , onNameBinding = return
                        , onMethodReference = onMethodReference' }
  onMethodReference' si (UnresolvedMethodReference protocol method) type' = do
    implementationMethod <- lookupMethodImplementation (unwrap si) protocol method
    resolvedMethod <- resolveInMethodImplementation implementationMethod
    return (si, ResolvedMethodReference protocol method resolvedMethod, type')
  onMemberAccess' = \case
    Core.RecordFieldAccess expr identifier ->
      Resolved.RecordFieldAccess <$> resolveInExpr' expr <*> return identifier
    Core.PackageMemberAccess pkg member ->
      return (Resolved.PackageMemberAccess pkg member)

resolveInDefinition' :: TypedDefinition -> Resolve ResolvedDefinition
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
    ImplementationDefinition si implementation -> do
      modify ((:) implementation)
      return undefined

runResolve :: ResolutionEnvironment -> Resolve a -> Either ResolutionError a
runResolve env = runExcept . flip evalStateT env

resolveInExpr :: ResolutionEnvironment
              -> TypedExpr
              -> Either ResolutionError ResolvedExpr
resolveInExpr env expr = runResolve env (resolveInExpr' expr)

resolveInDefinition :: ResolutionEnvironment
                    -> TypedDefinition
                    -> Either ResolutionError ResolvedDefinition
resolveInDefinition env def = runResolve env (resolveInDefinition' def)

resolveInDefinitions :: ResolutionEnvironment
                     -> [TypedDefinition]
                     -> Either ResolutionError [ResolvedDefinition]
resolveInDefinitions env defs = runResolve env (mapM resolveInDefinition' defs)

resolveInPackage :: ResolutionEnvironment
                 -> TypedPackage
                 -> Either ResolutionError ResolvedPackage
resolveInPackage env (TypedPackage decl imports defs) =
  ResolvedPackage decl imports <$> resolveInDefinitions env defs
