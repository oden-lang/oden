{-# LANGUAGE LambdaCase #-}
module Oden.Core.Package where

import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.QualifiedName (PackageName)

data PackageDeclaration
  = PackageDeclaration { packageDeclarationSourceInfo :: Metadata SourceInfo
                       , packageDeclarationName :: PackageName
                       }
  deriving (Show, Eq, Ord)

data ImportReference
  = ImportReference (Metadata SourceInfo) [String]
  | ImportForeignReference (Metadata SourceInfo) String
  deriving (Show, Eq, Ord)

instance HasSourceInfo ImportReference where
  getSourceInfo =
    \case
      ImportReference (Metadata si) _ -> si
      ImportForeignReference (Metadata si) _ -> si
  setSourceInfo si =
    \case
      ImportReference _ pkgName ->
        ImportReference (Metadata si) pkgName
      ImportForeignReference _ pkgPath ->
        ImportForeignReference (Metadata si) pkgPath

data ImportedPackage p
  = ImportedPackage ImportReference Identifier p
  deriving (Show, Eq, Ord)
