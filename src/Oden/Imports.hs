{-# LANGUAGE LambdaCase #-}
module Oden.Imports
  ( UnsupportedMessage
  , UnsupportedTypesWarning(..)
  , PackageImportError(..)
  , ForeignImporter
  , ImportResolutionEnvironment
  , resolveImports
  ) where

import           Oden.Core.Package
import           Oden.Core.Untyped
import           Oden.Core.Typed
import           Oden.Identifier
import           Oden.QualifiedName
import           Oden.SourceInfo

import Control.Monad.Except

import Control.Arrow (left)

import           Data.Map (Map)
import qualified Data.Map as Map

type UnsupportedMessage = (Identifier, String)
data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg      :: String
                                                       , messages :: [UnsupportedMessage]
                                                       } deriving (Show, Eq)

data PackageImportError
  = PackageNotFound SourceInfo PackageName
  | ForeignPackageImportError String String
  | IllegalImportPackageName SourceInfo IdentifierValidationError
  | PackageNotInEnvironment ImportReference
  deriving (Show, Eq, Ord)


type ImportResolutionEnvironment =
  Map
  ImportReference
  (TypedPackage, [UnsupportedMessage])


type ForeignImporter =
  String -> IO (Either PackageImportError (TypedPackage, [UnsupportedMessage]))


importedPackageIdentifier :: SourceInfo
                          -> PackageName
                          -> Either PackageImportError Identifier
importedPackageIdentifier si pkgName =
  left (IllegalImportPackageName si) legalIdentifier
  where
  legalIdentifier =
    case pkgName of
      NativePackageName segments ->
        createLegalIdentifier (last segments)
      ForeignPackageName name ->
        createLegalIdentifier name


findInEnv :: ImportReference
          -> ImportResolutionEnvironment
          -> Either PackageImportError (TypedPackage, [UnsupportedMessage])
findInEnv ref env =
  case Map.lookup ref env of
    Just x -> Right x
    Nothing -> Left (PackageNotInEnvironment ref)


resolveImports :: ImportResolutionEnvironment
               -> UntypedPackage ImportReference
               -> Either PackageImportError ( UntypedPackage (ImportedPackage TypedPackage)
                                           , [UnsupportedTypesWarning])
resolveImports env (UntypedPackage pkgDecl imports defs) = do
  (importedPackages, warnings) <- foldM resolveImport' ([], []) imports
  return (UntypedPackage pkgDecl importedPackages defs, warnings)
  where
  resolveImport' :: ([ImportedPackage TypedPackage], [UnsupportedTypesWarning])
                -> ImportReference
                -> Either PackageImportError ( [ImportedPackage TypedPackage]
                                            , [UnsupportedTypesWarning])
  resolveImport' (pkgs, warnings) ref = do
    (pkg'@(TypedPackage decl _ _), unsupported) <- findInEnv ref env
    let pkgName = packageDeclarationName decl
    pkgIdentifier <- importedPackageIdentifier (getSourceInfo ref) pkgName
    let warnings' =
          if null unsupported
          then warnings
          else UnsupportedTypesWarning (asString pkgIdentifier) unsupported : warnings
    return (ImportedPackage ref pkgIdentifier pkg' : pkgs, warnings')
