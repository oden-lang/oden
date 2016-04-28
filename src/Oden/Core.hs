{-# LANGUAGE TypeSynonymInstances #-}
module Oden.Core where

import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
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

data Definition e
  = Definition (Metadata SourceInfo) Identifier e
  | ForeignDefinition (Metadata SourceInfo) Identifier Poly.Scheme
  | TypeDefinition (Metadata SourceInfo) QualifiedName [NameBinding] Poly.Type
  | ProtocolDefinition (Metadata SourceInfo) QualifiedName Poly.Protocol
  | ImplementationDefinition (Metadata SourceInfo) (ProtocolImplementation TypedExpr)
  deriving (Show, Eq, Ord)

type TypedDefinition = Definition CanonicalExpr

data ImportedPackage = ImportedPackage (Metadata SourceInfo) Identifier TypedPackage
                     deriving (Show, Eq, Ord)

-- Some handy aliases.
type TypedExpr = Expr UnresolvedMethodReference Poly.Type TypedMemberAccess
type TypedRange = Range TypedExpr
type TypedPackage = Package ImportedPackage TypedDefinition
