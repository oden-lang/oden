module Oden.ImportsSpec where

import Oden.Core.Package
import Oden.Core.Typed
import Oden.Core.Untyped

import Oden.Imports
import Oden.Metadata
import Oden.QualifiedName
import Oden.SourceInfo

import Test.Hspec


missing :: Metadata SourceInfo
missing = Metadata Missing


pkgWithImports :: [i] -> UntypedPackage i
pkgWithImports imports =
  UntypedPackage (PackageDeclaration missing (NativePackageName ["mypkg"])) imports []


typedPkg :: PackageName -> TypedPackage
typedPkg name =
  TypedPackage (PackageDeclaration missing name) [] []


-- | Creates a foreign importer that always returns an empty package with the
-- given package name.
foreignImporter :: String -> ForeignImporter
foreignImporter packageName _pkgPath =
  return $ Right $ (typedPkg (ForeignPackageName packageName), [])


spec :: Spec
spec = describe "resolveImports" $
  it "validations the imported package identifier" pending
