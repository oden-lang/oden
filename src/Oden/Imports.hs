{-# LANGUAGE LambdaCase #-}
module Oden.Imports
  ( UnsupportedMessage
  , UnsupportedTypesWarning(..)
  , PackageImportError(..)
  , ForeignImporter
  , NativeImporter
  , resolveImports
  ) where

import           Oden.Core.Package
import           Oden.Core.Untyped
import           Oden.Core.Typed
import           Oden.Identifier

import Control.Monad.Except

type UnsupportedMessage = (Identifier, String)
data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg      :: PackageName
                                                       , messages :: [UnsupportedMessage]
                                                       } deriving (Show, Eq)

data PackageImportError
  = NativePackageNotFound PackageName
  | ForeignPackageImportError String String deriving (Show, Eq)


type Imports = ExceptT PackageImportError IO


type ForeignImporter =
  String -> IO (Either PackageImportError (TypedPackage, [UnsupportedMessage]))


type NativeImporter =
  PackageName -> IO (Either PackageImportError TypedPackage)


liftEither :: Either PackageImportError b -> Imports b
liftEither = either throwError return

resolveImports' :: ForeignImporter
                -> NativeImporter
                -> UntypedPackage ImportReference
                -> Imports (UntypedPackage (ImportedPackage TypedPackage), [UnsupportedTypesWarning])
resolveImports' foreignImporter nativeImporter (UntypedPackage pkgDecl imports defs) = do
  (importedPackages, warnings) <- foldM resolveImport ([], []) imports
  return (UntypedPackage pkgDecl importedPackages defs, warnings)
  where
  resolveImport :: ([ImportedPackage TypedPackage], [UnsupportedTypesWarning])
                -> ImportReference
                -> Imports ([ImportedPackage TypedPackage], [UnsupportedTypesWarning])
  resolveImport (pkgs, warnings) =
    \case
      ImportForeignReference sourceInfo pkgString -> do
        res <- liftIO $ foreignImporter pkgString
        (pkg'@(TypedPackage (PackageDeclaration _ pkgName) _ _), unsupported) <- liftEither res
        let warnings' =
              if null unsupported
              then warnings
              else UnsupportedTypesWarning pkgName unsupported : warnings
        return (ImportedPackage sourceInfo (Identifier (last pkgName)) pkg' : pkgs, warnings')
      ImportReference sourceInfo importedPkgName -> do
        res <- liftIO $ nativeImporter importedPkgName
        pkg'@(TypedPackage (PackageDeclaration _ pkgName) _ _) <- liftEither res
        return (ImportedPackage sourceInfo (Identifier (last pkgName)) pkg' : pkgs, warnings)


resolveImports :: ForeignImporter
               -> NativeImporter
               -> UntypedPackage ImportReference
               -> IO (Either PackageImportError (UntypedPackage (ImportedPackage TypedPackage),
                                                [UnsupportedTypesWarning]))
resolveImports foreignImporter nativeImporter pkg' =
  runExceptT (resolveImports' foreignImporter nativeImporter pkg')
