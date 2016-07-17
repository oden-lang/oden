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
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo

import Control.Monad.Except

import Control.Arrow (left)

type UnsupportedMessage = (Identifier, String)
data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg      :: String
                                                       , messages :: [UnsupportedMessage]
                                                       } deriving (Show, Eq)

data PackageImportError
  = PackageNotFound PackageName
  | ForeignPackageImportError String String
  | IllegalImportPackageName SourceInfo IdentifierValidationError
  deriving (Show)


type Imports = ExceptT PackageImportError IO


type ForeignImporter =
  String -> IO (Either PackageImportError (TypedPackage, [UnsupportedMessage]))


type NativeImporter =
  [String] -> IO (Either PackageImportError TypedPackage)


liftEither :: Either PackageImportError b -> Imports b
liftEither = either throwError return


importedPackageIdentifier :: SourceInfo -> PackageName -> Imports Identifier
importedPackageIdentifier si pkgName =
  liftEither (left (IllegalImportPackageName si) legalIdentifier)
  where
  legalIdentifier =
    case pkgName of
      NativePackageName segments ->
        createLegalIdentifier (last segments)
      ForeignPackageName name ->
        createLegalIdentifier name


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
  resolveImport (pkgs, warnings) ref =
    case ref of
      ImportForeignReference sourceInfo pkgString -> do
        res <- liftIO $ foreignImporter pkgString
        (pkg'@(TypedPackage decl _ _), unsupported) <- liftEither res
        let pkgName = packageDeclarationName decl
        pkgIdentifier <- importedPackageIdentifier (unwrap sourceInfo) pkgName
        let warnings' =
              if null unsupported
              then warnings
              else UnsupportedTypesWarning (asString pkgIdentifier) unsupported : warnings
        return (ImportedPackage ref pkgIdentifier pkg' : pkgs, warnings')
      ImportReference sourceInfo importedPkgName -> do
        res <- liftIO $ nativeImporter importedPkgName
        pkg'@(TypedPackage decl _ _) <- liftEither res
        let pkgName = packageDeclarationName decl
        pkgIdentifier <- importedPackageIdentifier (unwrap sourceInfo) pkgName
        return (ImportedPackage ref pkgIdentifier pkg' : pkgs, warnings)


resolveImports :: ForeignImporter
               -> NativeImporter
               -> UntypedPackage ImportReference
               -> IO (Either PackageImportError (UntypedPackage (ImportedPackage TypedPackage),
                                                [UnsupportedTypesWarning]))
resolveImports foreignImporter nativeImporter pkg' =
  runExceptT (resolveImports' foreignImporter nativeImporter pkg')
