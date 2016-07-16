module Oden.Core.Package where

import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo

type PackageName = [String]

data PackageDeclaration = PackageDeclaration (Metadata SourceInfo) PackageName
                          deriving (Show, Eq, Ord)

data ImportReference
  = ImportReference (Metadata SourceInfo) PackageName
  | ImportForeignReference (Metadata SourceInfo) String
  deriving (Show, Eq, Ord)

data ImportedPackage p
  = ImportedPackage (Metadata SourceInfo) Identifier p
  deriving (Show, Eq, Ord)
