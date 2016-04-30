module Oden.Core.Resolved where

import Oden.Core.Expr
import Oden.Core.Definition
import Oden.Core.Package
import Oden.Core.ProtocolImplementation
import Oden.Identifier
import Oden.Type.Polymorphic

data ResolvedMethodReference
  = ResolvedMethodReference Protocol ProtocolMethod (MethodImplementation ResolvedExpr)
  deriving (Show, Eq, Ord)

data ResolvedMemberAccess
  = RecordFieldAccess ResolvedExpr Identifier
  | PackageMemberAccess Identifier Identifier
  deriving (Show, Eq, Ord)

type ResolvedExpr = Expr ResolvedMethodReference Type ResolvedMemberAccess
type ResolvedDefinition = Definition ResolvedExpr

data ResolvedPackage
  = ResolvedPackage PackageDeclaration [ImportedPackage ResolvedPackage] [ResolvedDefinition]
  deriving (Show, Eq, Ord)
