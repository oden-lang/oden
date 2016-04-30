module Oden.Core.Monomorphed where

import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.Resolved        hiding (ResolvedMemberAccess(..))
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic     as Mono

import           Data.Set                  as Set hiding (map)

data MonoTypedMemberAccess
  = RecordFieldAccess MonoTypedExpr Identifier
  | PackageMemberAccess Identifier Identifier
  deriving (Show, Eq, Ord)

type MonoTypedExpr = Expr ResolvedMethodReference Mono.Type MonoTypedMemberAccess
type MonoTypedRange = Range MonoTypedExpr

data MonomorphedDefinition = MonomorphedDefinition (Metadata SourceInfo) Identifier Mono.Type MonoTypedExpr
                           deriving (Show, Eq, Ord)

data InstantiatedDefinition =
  InstantiatedDefinition Identifier (Metadata SourceInfo) Identifier MonoTypedExpr
  deriving (Show, Eq, Ord)

data MonomorphedPackage = MonomorphedPackage PackageDeclaration
                                             [ImportedPackage ResolvedPackage]
                                             (Set InstantiatedDefinition)
                                             (Set MonomorphedDefinition)
                     deriving (Show, Eq, Ord)

