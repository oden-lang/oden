{-# LANGUAGE TypeSynonymInstances #-}
module Oden.Core where

import           Oden.Core.Expr
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName (QualifiedName(..))
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic as Poly

data UnresolvedMethodReference
  = UnresolvedMethodReference Poly.Protocol Poly.ProtocolMethod
  deriving (Show, Eq, Ord)

-- data ResolvedMethodReference = ResolvedMethodReference Poly.Protocol Poly.ProtocolMethod

type CanonicalExpr = (Poly.Scheme, TypedExpr)

data Definition = Definition (Metadata SourceInfo) Identifier CanonicalExpr
                | ForeignDefinition (Metadata SourceInfo) Identifier Poly.Scheme
                | TypeDefinition (Metadata SourceInfo) QualifiedName [NameBinding] Poly.Type
                | ProtocolDefinition (Metadata SourceInfo) QualifiedName Poly.Protocol
                deriving (Show, Eq, Ord)

type PackageName = [String]

data PackageDeclaration = PackageDeclaration (Metadata SourceInfo) PackageName
                        deriving (Show, Eq, Ord)

data ImportedPackage = ImportedPackage (Metadata SourceInfo) Identifier Package
                     deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [ImportedPackage] [Definition]
             deriving (Show, Eq, Ord)

-- Some handy aliases.
type TypedExpr = Expr UnresolvedMethodReference Poly.Type
type TypedRange = Range TypedExpr
