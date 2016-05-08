module Oden.Core.Typed where

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Identifier
import qualified Oden.Type.Polymorphic as Poly

data TypedMemberAccess
  = RecordFieldAccess TypedExpr Identifier
  | PackageMemberAccess Identifier Identifier
  deriving (Show, Eq, Ord)

data TypedMethodReference
  = Unresolved Poly.ProtocolName Poly.MethodName
  | Resolved Poly.ProtocolName Poly.MethodName (MethodImplementation TypedExpr)
  deriving (Show, Eq, Ord)

type CanonicalExpr = (Poly.Scheme, TypedExpr)

type TypedDefinition = Definition TypedExpr

-- Some handy aliases.
type TypedExpr = Expr TypedMethodReference Poly.Type TypedMemberAccess
type TypedRange = Range TypedExpr

data TypedPackage
  = TypedPackage PackageDeclaration [ImportedPackage TypedPackage] [TypedDefinition]
  deriving (Show, Eq, Ord)
