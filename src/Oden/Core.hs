{-# LANGUAGE TypeSynonymInstances #-}
module Oden.Core where

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Resolved
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName (QualifiedName(..))
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic as Poly

data TypedMemberAccess
  = RecordFieldAccess TypedExpr Identifier
  | PackageMemberAccess Identifier Identifier
  deriving (Show, Eq, Ord)

data UnresolvedMethodReference
  = UnresolvedMethodReference Poly.Protocol Poly.ProtocolMethod
  deriving (Show, Eq, Ord)

type CanonicalExpr = (Poly.Scheme, TypedExpr)

type TypedDefinition = Definition TypedExpr

-- Some handy aliases.
type TypedExpr = Expr UnresolvedMethodReference Poly.Type TypedMemberAccess
type TypedRange = Range TypedExpr

data TypedPackage
  = TypedPackage PackageDeclaration [ImportedPackage ResolvedPackage] [TypedDefinition]
  deriving (Show, Eq, Ord)
