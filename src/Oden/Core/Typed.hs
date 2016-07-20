module Oden.Core.Typed where

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Identifier
import           Oden.Type.Polymorphic


data TypedMemberAccess
  = RecordFieldAccess TypedExpr Identifier
  | PackageMemberAccess Identifier Identifier
  deriving (Show, Eq, Ord)


data TypedMethodReference
  = Unresolved ProtocolName MethodName ProtocolConstraint
  | Resolved ProtocolName MethodName (MethodImplementation TypedExpr)
  deriving (Show, Eq, Ord)


type CanonicalExpr = (Scheme, TypedExpr)


type TypedDefinition = Definition TypedExpr


type TypedExpr = Expr TypedMethodReference Type TypedMemberAccess
type TypedRange = Range TypedExpr


data TypedPackage
  = TypedPackage PackageDeclaration [ImportedPackage TypedPackage] [TypedDefinition]
  deriving (Show, Eq, Ord)
