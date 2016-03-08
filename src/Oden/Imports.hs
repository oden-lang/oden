module Oden.Imports where

import qualified Oden.Core                       as Core
import qualified Oden.Core.Untyped               as Untyped
import           Oden.Identifier

import Control.Monad.Except

type UnsupportedMessage = (Identifier, String)
data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg      :: Core.PackageName
                                                       , messages :: [UnsupportedMessage]
                                                       } deriving (Show, Eq)

data PackageImportError = PackageImportError Core.PackageName String deriving (Show, Eq)


type Imports = ExceptT PackageImportError IO

type Importer = Core.PackageName
                -> IO (Either PackageImportError (Core.Package, [UnsupportedMessage]))

resolveImports' :: Importer
                -> Untyped.Package [Untyped.ImportReference]
                -> Imports (Untyped.Package [Core.ImportedPackage], [UnsupportedTypesWarning])
resolveImports' importer (Untyped.Package pkgDecl@(Untyped.PackageDeclaration _ _) imports defs) = do
  (importedPackages, warnings) <- foldM resolveImport ([], []) imports
  return (Untyped.Package pkgDecl importedPackages defs, warnings)
  where
  resolveImport :: ([Core.ImportedPackage], [UnsupportedTypesWarning])
                -> Untyped.ImportReference
                -> Imports ([Core.ImportedPackage], [UnsupportedTypesWarning])
  resolveImport (pkgs, warnings) (Untyped.ImportReference sourceInfo importedPkgName) = do
    result <- liftIO $ importer importedPkgName
    (pkg', unsupported) <- either throwError return result
    let warnings' = if null unsupported then warnings else UnsupportedTypesWarning importedPkgName unsupported : warnings
    return (Core.ImportedPackage sourceInfo (Identifier (last importedPkgName)) pkg' : pkgs, warnings')

resolveImports :: Importer
               -> Untyped.Package [Untyped.ImportReference]
               -> IO (Either PackageImportError (Untyped.Package [Core.ImportedPackage], [UnsupportedTypesWarning]))
resolveImports importer pkg' = runExceptT (resolveImports' importer pkg')
