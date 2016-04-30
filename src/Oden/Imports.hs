module Oden.Imports where

import           Oden.Core.Package
import           Oden.Core.Untyped
import           Oden.Core.Resolved
import           Oden.Identifier

import Control.Monad.Except

type UnsupportedMessage = (Identifier, String)
data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg      :: PackageName
                                                       , messages :: [UnsupportedMessage]
                                                       } deriving (Show, Eq)

data PackageImportError = PackageImportError PackageName String deriving (Show, Eq)


type Imports = ExceptT PackageImportError IO

type Importer =
  PackageName -> IO (Either PackageImportError
                            (ResolvedPackage, [UnsupportedMessage]))

resolveImports' :: Importer
                -> UntypedPackage ImportReference
                -> Imports (UntypedPackage (ImportedPackage ResolvedPackage), [UnsupportedTypesWarning])
resolveImports' importer (UntypedPackage pkgDecl@(PackageDeclaration _ _) imports defs) = do
  (importedPackages, warnings) <- foldM resolveImport ([], []) imports
  return (UntypedPackage pkgDecl importedPackages defs, warnings)
  where
  resolveImport :: ([ImportedPackage ResolvedPackage], [UnsupportedTypesWarning])
                -> ImportReference
                -> Imports ([ImportedPackage ResolvedPackage], [UnsupportedTypesWarning])
  resolveImport (pkgs, warnings) (ImportReference sourceInfo importedPkgName) = do
    result <- liftIO $ importer importedPkgName
    (pkg', unsupported) <- either throwError return result
    let warnings' = if null unsupported then warnings else UnsupportedTypesWarning importedPkgName unsupported : warnings
    return (ImportedPackage sourceInfo (Identifier (last importedPkgName)) pkg' : pkgs, warnings')

resolveImports :: Importer
               -> UntypedPackage ImportReference
               -> IO (Either PackageImportError (UntypedPackage (ImportedPackage ResolvedPackage),
                                                 [UnsupportedTypesWarning]))
resolveImports importer pkg' = runExceptT (resolveImports' importer pkg')
