module Oden.Core.Monomorphed where

import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.Typed
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono

import           Data.Set              as Set hiding (map)

data MonoTypedMemberAccess
  = RecordFieldAccess MonoTypedExpr Identifier
  | PackageMemberAccess Identifier Identifier
  deriving (Show, Eq, Ord)

type MonoTypedExpr = Expr TypedMethodReference Mono.Type MonoTypedMemberAccess
type MonoTypedRange = Range MonoTypedExpr

data MonomorphedDefinition = MonomorphedDefinition (Metadata SourceInfo) Identifier Mono.Type MonoTypedExpr
                           deriving (Show, Eq, Ord)

data InstantiatedDefinition
  = InstantiatedDefinition Identifier (Metadata SourceInfo) Identifier MonoTypedExpr
  | InstantiatedMethod (Metadata SourceInfo) Identifier MonoTypedExpr
  deriving (Show, Eq, Ord)

data MonomorphedPackage = MonomorphedPackage PackageDeclaration
                                             [ImportedPackage TypedPackage]
                                             (Set InstantiatedDefinition)
                                             (Set MonomorphedDefinition)
                     deriving (Show, Eq, Ord)

