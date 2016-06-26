{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
-- | Resolves protocol method usage to implementation if there's a single
-- possible resolution that matches. Note that we only make local protocol
-- implementations available for now.
module Oden.Compiler.Resolution (
  ResolutionEnvironment,
  ResolutionError(..),
  resolveInPackage
  ) where

import           Oden.Core.Definition
import           Oden.Core.Expr                   (mapType)
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Traversal
import           Oden.Core.Typed

import           Oden.Infer.Unification

import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Control.Monad.Except
import           Control.Monad.Trans.RWS

import           Data.Set                         (Set)
import qualified Data.Set                         as Set

data ResolutionError
  = NoMatchingImplementationInScope SourceInfo ProtocolName Type [ProtocolImplementation TypedExpr]
  | MultipleMatchingImplementationsInScope SourceInfo [ProtocolImplementation TypedExpr]
  deriving (Show, Eq, Ord)

type ResolutionEnvironment = Set (ProtocolImplementation TypedExpr)

type Resolve = RWST
               ()
               (Set ProtocolConstraint)
               ResolutionEnvironment
               (Except ResolutionError)


resolveImplementation :: ProtocolConstraint -> Resolve (Maybe (ProtocolImplementation TypedExpr))
resolveImplementation =
  \case
    ProtocolConstraint _ _ TVar{} -> return Nothing
    ProtocolConstraint (Metadata si) protocolName' type' -> do
      allImpls <- gets Set.toList
      case filter implements allImpls of
        []     -> throwError (NoMatchingImplementationInScope si protocolName' type' allImpls)
        [impl] -> return (Just impl)
        impls  -> throwError (MultipleMatchingImplementationsInScope si impls)
      where
      implements (ProtocolImplementation _ implName implType _) =
        implName == protocolName' && implType `unifiesWith` type'


resolveInMethodImplementation :: MethodImplementation TypedExpr
                              -> Resolve (MethodImplementation TypedExpr)
resolveInMethodImplementation (MethodImplementation si method expr) =
  MethodImplementation si method <$> resolveInExpr' expr


resolveInImplementation :: ProtocolImplementation TypedExpr
                        -> Resolve (ProtocolImplementation TypedExpr)
resolveInImplementation (ProtocolImplementation si protocol implHead methods) =
  ProtocolImplementation si protocol implHead <$> mapM resolveInMethodImplementation methods


findMethod :: ProtocolImplementation TypedExpr
           -> MethodName
           -> Resolve (MethodImplementation TypedExpr)
findMethod (ProtocolImplementation _ _ _ methods) name =
  case filter matchesName methods of
    [method] -> return method
    _ -> error ("expected method to be present in implementation: " ++ show name)
  where
  matchesName (MethodImplementation _ methodName _) =
    methodName == name


resolveInExpr' :: TypedExpr -> Resolve TypedExpr
resolveInExpr' = traverseExpr traversal
  where
  traversal = Traversal { onExpr = const (return Nothing)
                        , onType = onType'
                        , onMemberAccess = onMemberAccess'
                        , onNameBinding = return
                        , onMethodReference = onMethodReference' }
  onType' t@(TConstrained _constraints _) = --do
    -- resolvedConstraints <- filterM isResolvable (Set.toList constraints)
    return t -- (dropConstraints t (Set.toList resolvedConstraints))
    --where
    --isResolvable constraint = isJust <$> resolveImplementation constraint
  onType' t = return t
  onMethodReference' si reference methodType =
    case reference of
      Unresolved protocolName' methodName constraint -> do
        impl <- resolveImplementation constraint
        case impl of
          Just impl' -> do
            method <- findMethod impl' methodName
            let constraints = Set.singleton constraint
            tell constraints
            -- As we have resolved an implementation for this constraint, we
            -- can remove it the from type.
            return (si,
                    Resolved protocolName' methodName method,
                    dropConstraints methodType constraints)
          Nothing ->
            return (si, reference, methodType)
      Resolved{} ->
        return (si, reference, methodType)
  onMemberAccess' = \case
    RecordFieldAccess expr identifier ->
      RecordFieldAccess <$> resolveInExpr' expr <*> return identifier
    PackageMemberAccess pkg member ->
      return (PackageMemberAccess pkg member)


resolveInDefinition :: TypedDefinition -> Resolve TypedDefinition
resolveInDefinition =
  \case
    Definition si name (scheme, expr) -> do
      (resolvedExpr, resolvedConstraints) <- listen (resolveInExpr' expr)
      return (Definition
              si
              name
              (dropConstraints scheme resolvedConstraints,
               mapType (`dropConstraints` resolvedConstraints) resolvedExpr))
    ForeignDefinition si name scheme ->
      return (ForeignDefinition si name scheme)
    TypeDefinition si name bindings type' ->
      return (TypeDefinition si name bindings type')
    ProtocolDefinition si name protocol ->
      return (ProtocolDefinition si name protocol)
    Implementation si implementation -> do
      resolved <- resolveInImplementation implementation
      modify (Set.insert resolved . Set.delete implementation)
      return (Implementation si resolved)


runResolve :: ResolutionEnvironment -> Resolve a -> Either ResolutionError a
runResolve env r = runExcept (fst <$> evalRWST r () env)


resolveInDefinitions :: ResolutionEnvironment
                     -> [TypedDefinition]
                     -> Either ResolutionError [TypedDefinition]
resolveInDefinitions env defs = runResolve env (mapM resolveInDefinition defs)


resolveInPackage :: ResolutionEnvironment
                 -> TypedPackage
                 -> Either ResolutionError TypedPackage
resolveInPackage env (TypedPackage decl imports defs) =
  TypedPackage decl imports <$> resolveInDefinitions env defs
