module Oden.Core.Package where

import           Oden.Metadata
import           Oden.SourceInfo

type PackageName = [String]

data PackageDeclaration = PackageDeclaration (Metadata SourceInfo) PackageName
                          deriving (Show, Eq, Ord)

data ImportReference = ImportReference (Metadata SourceInfo) PackageName
                     deriving (Show, Eq, Ord)

data Package i d
  = Package PackageDeclaration [i] [d]
  deriving (Show, Eq, Ord)
