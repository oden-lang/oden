{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Resolves protocol method usage to implementation if there's a single
-- possible resolution that matches.
module Oden.Compiler.Resolution where

import Oden.Core
import Oden.Core.Expr
import Oden.Core.ProtocolImplementation
import Oden.Core.Traversal
import Oden.Compiler.Resolution.Environment
import Oden.Type.Polymorphic

import Control.Monad.Except
import Control.Monad.Reader

data ResolvedMethodReference
  = ResolvedMethodReference Protocol ProtocolMethod (MethodImplementation TypedExpr)
  deriving (Show, Eq, Ord)

type ResolvedExpr = Expr ResolvedMethodReference Type TypedMemberAccess
type ResolvedDefinition = Definition (Scheme, ResolvedExpr)

data ResolutionError
  = NoMatchingImplementationInScope
  deriving (Show, Eq, Ord)

type Resolve = ReaderT ResolutionEnvironment (Except ResolutionError)

resolveInExpr :: TypedExpr -> Resolve ResolvedExpr
resolveInExpr = traverseExpr traversal
  where
  traversal = Traversal { onExpr = onExpr'
                        , onType = return
                        , onMemberAccess = return
                        , onNameBinding = return
                        , onMethodReference = onMethodReference' }
  onExpr' :: TypedExpr -> Resolve (Maybe ResolvedExpr)
  onExpr' = \case
    MethodReference si reference type' ->
      Just <$> (MethodReference si <$> onMethodReference' reference <*> return type')
    _ -> return Nothing
  onMethodReference' (UnresolvedMethodReference _protocol _method) =
    throwError NoMatchingImplementationInScope

resolveInDefinition :: ResolutionEnvironment
                    -> TypedDefinition
                    -> Either ResolutionError ResolvedDefinition
resolveInDefinition env = \case
  Definition si name (scheme, expr) -> do
    expr' <- runExcept (runReaderT (resolveInExpr expr) env)
    return (Definition si name (scheme, expr'))
  ForeignDefinition si name scheme ->
    return (ForeignDefinition si name scheme)
  TypeDefinition si name bindings type' ->
    return (TypeDefinition si name bindings type')
  ProtocolDefinition si name protocol ->
    return (ProtocolDefinition si name protocol)
  ImplementationDefinition si implementation ->
    return (ImplementationDefinition si implementation)
