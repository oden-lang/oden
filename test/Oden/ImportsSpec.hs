module Oden.ImportsSpec where

import Oden.Core.Package
import Oden.Core.Typed
import Oden.Core.Untyped

import Oden.Assertions
import Oden.Imports
import Oden.Identifier
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


nativeImporter :: NativeImporter
nativeImporter pkgName = return $ Right $ typedPkg (NativePackageName pkgName)


spec :: Spec
spec = describe "resolveImports" $ do

  it "handles no imports" $ do
    pkg' <- resolveImports
          (foreignImporter [])
          nativeImporter
          (pkgWithImports [])
    pkg' `shouldSucceedWith` (pkgWithImports [], [])

  it "resolves native imports" $ do
    pkg' <- resolveImports
          (foreignImporter [])
          nativeImporter
          (pkgWithImports [ImportReference missing ["some", "pkg"]])
    let resolved = [ImportedPackage
                    (ImportReference missing ["some", "pkg"])
                    (Identifier "pkg")
                    (typedPkg (NativePackageName ["some", "pkg"]))]
    pkg' `shouldSucceedWith` (pkgWithImports resolved, [])

  it "resolves foreign imports" $ do
    pkg' <- resolveImports
          (foreignImporter "pkg")
          nativeImporter
          (pkgWithImports [ImportForeignReference missing "some/pkg"])
    let resolved = [ImportedPackage
                    (ImportForeignReference missing "some/pkg")
                    (Identifier "pkg")
                    (typedPkg (ForeignPackageName "pkg"))]
    pkg' `shouldSucceedWith` (pkgWithImports resolved, [])
