{-# LANGUAGE LambdaCase #-}
-- | Resolves protocol method usage to implementation if there's a single
-- possible resolution that matches.
module Oden.Compiler.Resolution where

import Oden.Core
import Oden.Core.Expr
import Oden.Core.Traversal
import Oden.Compiler.Resolution.Environment
import Oden.Type.Polymorphic

import Control.Monad.Except
import Control.Monad.Reader

data ResolvedMethodReference
  = ResolvedMethodReference Protocol ProtocolMethod
  deriving (Show, Eq, Ord)

type ResolvedExpr = Expr UnresolvedMethodReference Type TypedMemberAccess
type ResolvedDefinition = Definition (Scheme, ResolvedExpr)

data ResolutionError
  = NoMatchingImplementationInScope
  deriving (Show, Eq, Ord)

type Resolve = ReaderT ResolutionEnvironment (Except ResolutionError)

resolveInExpr :: TypedExpr -> Resolve ResolvedExpr
resolveInExpr = traverseExpr identityTraversal { onExpr = onExpr' }
  where
  onExpr' = \case
    MethodReference _ (UnresolvedMethodReference _protocol _method) _t ->
      throwError NoMatchingImplementationInScope
    _ -> return Nothing

resolveInDefinition :: ResolutionEnvironment
                    -> TypedDefinition
                    -> Either ResolutionError ResolvedDefinition
resolveInDefinition env = \case
  Definition si name (scheme, expr) -> do
    expr' <- runExcept (runReaderT (resolveInExpr expr) env)
    return (Definition si name (scheme, expr'))
  def -> return def
