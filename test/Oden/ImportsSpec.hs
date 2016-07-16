module Oden.ImportsSpec where

import Oden.Core.Package
import Oden.Core.Typed
import Oden.Core.Untyped

import Oden.Assertions
import Oden.Imports
import Oden.Identifier
import Oden.Metadata
import Oden.SourceInfo

import Test.Hspec


missing :: Metadata SourceInfo
missing = Metadata Missing


pkgWithImports :: [i] -> UntypedPackage i
pkgWithImports imports =
  UntypedPackage (PackageDeclaration missing ["mypkg"]) imports []


typedPkg :: PackageName -> TypedPackage
typedPkg name =
  TypedPackage (PackageDeclaration missing name) [] []

-- | Creates a foreign importer that always returns an empty package with the
-- given package name.
foreignImporter :: PackageName -> ForeignImporter
foreignImporter packageName pkgPath = return $ Right $ (typedPkg packageName, [])


nativeImporter :: NativeImporter
nativeImporter pkgName = return $ Right $ typedPkg pkgName


spec :: Spec
spec = describe "resolveImports" $ do

  it "handles no imports" $ do
    pkg <- resolveImports
          (foreignImporter [])
          nativeImporter
          (pkgWithImports [])
    pkg `shouldSucceedWith` (pkgWithImports [], [])

  it "resolves native imports" $ do
    pkg <- resolveImports
          (foreignImporter [])
          nativeImporter
          (pkgWithImports [ImportReference missing ["some", "pkg"]])
    let resolved = [ImportedPackage
                    missing
                    (Identifier "pkg")
                    (typedPkg ["some", "pkg"])]
    pkg `shouldSucceedWith` (pkgWithImports resolved, [])

  it "resolves foreign imports" $ do
    pkg <- resolveImports
          (foreignImporter ["some", "pkg"])
          nativeImporter
          (pkgWithImports [ImportForeignReference missing "some/pkg"])
    let resolved = [ImportedPackage
                    missing
                    (Identifier "pkg")
                    (typedPkg ["some", "pkg"])]
    pkg `shouldSucceedWith` (pkgWithImports resolved, [])
