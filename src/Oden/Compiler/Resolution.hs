{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Resolves protocol method usage to implementation if there's a single
-- possible resolution that matches. Note that we only make local protocol
-- implementations available for now.
module Oden.Compiler.Resolution where

import Oden.Core.Typed
import Oden.Core.Definition
import Oden.Core.ProtocolImplementation
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
lookupMethodImplementation si protocol method@(ProtocolMethod _ methodName _) = do
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
  matchesMethod (MethodImplementation _ (ProtocolMethod _ methodName' _) _) =
    methodName == methodName'
  selectSingleMethod :: ProtocolImplementation TypedExpr
                     -> Resolve (MethodImplementation TypedExpr)
  selectSingleMethod (ProtocolImplementation _ _ methodImpls) =
    case filter matchesMethod methodImpls of
      [] -> error "OMG no method impl"
      [methodImpl] -> return methodImpl
      _methodImpls -> error "OMG too many method impls"

resolveInMethodImplementation :: MethodImplementation TypedExpr
                              -> Resolve (MethodImplementation TypedExpr)
resolveInMethodImplementation (MethodImplementation si method expr) =
  MethodImplementation si method <$> resolveInExpr' expr

resolveInImplementation :: ProtocolImplementation TypedExpr
                        -> Resolve (ProtocolImplementation TypedExpr)
resolveInImplementation (ProtocolImplementation si protocol methods) =
  ProtocolImplementation si protocol <$> mapM resolveInMethodImplementation methods

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
        implementationMethod <- lookupMethodImplementation (unwrap si) protocol method
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
      modify ((:) resolved)
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
