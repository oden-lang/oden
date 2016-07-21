module Oden.CLI.Build where

import           Oden.Backend
import           Oden.Backend.Go

import           Oden.Compiler
import           Oden.Compiler.Environment
import           Oden.Compiler.Resolution
import           Oden.Compiler.Validation.Typed   as TypedValidation
import           Oden.Compiler.Validation.Untyped as UntypedValidation

import           Oden.Core.Monomorphed            (MonomorphedPackage (..))
import           Oden.Core.Package                (ImportReference(..))
import           Oden.Core.Typed
import           Oden.Core.Untyped                (UntypedPackage(..))

import           Oden.Desugar
import qualified Oden.Go.Importer                 as Go
import           Oden.Imports
import           Oden.Infer
import           Oden.Metadata
import           Oden.QualifiedName               (PackageName(NativePackageName))
import           Oden.Parser                      (parsePackage)
import           Oden.Path
import           Oden.Scanner
import           Oden.SourceFile
import qualified Oden.Syntax                      as Syntax

import           Oden.CLI

import           Control.Monad.Reader

import qualified Data.Text.Lazy.IO as L
import qualified Data.Map          as Map

import           System.Directory
import           System.FilePath


writeCompiledFile :: CompiledFile -> CLI ()
writeCompiledFile (CompiledFile name contents) =
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory name)
    writeFile name contents


readPackage :: FilePath -> CLI Syntax.Package
readPackage fname = do
  contents <- liftIO $ L.readFile fname
  liftEither $ parsePackage fname contents


validateUntypedPkg :: UntypedPackage ImportReference -> CLI ()
validateUntypedPkg pkg' = liftEither (UntypedValidation.validate pkg') >>= mapM_ logWarning


validateTypedPkg :: TypedPackage -> CLI ()
validateTypedPkg pkg' = liftEither (TypedValidation.validate pkg') >>= mapM_ logWarning


logCompiledFiles :: [CompiledFile] -> CLI ()
logCompiledFiles [_] = liftIO $ putStrLn "Compiled 1 Go source file."
logCompiledFiles files = liftIO $ putStrLn $ "Compiled " ++ show (length files) ++ " Go source files."


getImportResolutionEnv :: UntypedPackage ImportReference
                       -> CLI ImportResolutionEnvironment
getImportResolutionEnv (UntypedPackage _ imports _) =
  foldM addImport Map.empty imports
  where
    addImport :: ImportResolutionEnvironment
              -> ImportReference
              -> CLI ImportResolutionEnvironment
    addImport env ref =
      case ref of
        ImportReference si pkgName -> do
          opts <- ask
          foundPath <- liftIO (findPackageFileInPath pkgName (odenPath opts))
          case foundPath of
            Just path -> do
              let sourceFile = OdenSourceFile path (NativePackageName pkgName)
              typedPkg' <- inferAndValidateFile sourceFile
              return (Map.insert ref (typedPkg', []) env)

            Nothing -> liftEither (Left (PackageNotFound
                                        (unwrap si)
                                        (NativePackageName pkgName)))

        ImportForeignReference _ pkgPath -> do
          pkgAndWarnings <- liftIO (Go.importer pkgPath) >>= liftEither
          return (Map.insert ref pkgAndWarnings env)


inferFile :: SourceFile -> CLI (TypingEnvironment, TypedPackage)
inferFile (OdenSourceFile fname _) = do
  -- TODO: Check package name
  syntaxPkg <- readPackage fname
  untypedPkg <- liftEither' (desugarPackage syntaxPkg)
  validateUntypedPkg untypedPkg
  importEnv <- getImportResolutionEnv untypedPkg
  (untypedPkgWithImports, warnings') <- liftEither (resolveImports
                                                   importEnv
                                                   untypedPkg)
  mapM_ logWarning warnings'
  liftEither (inferPackage untypedPkgWithImports)


resolvePkg :: CompileEnvironment -> TypedPackage -> CLI TypedPackage
resolvePkg compileEnv pkg' =
  liftEither (resolveInPackage compileEnv pkg')


inferAndValidateFile :: SourceFile -> CLI TypedPackage
inferAndValidateFile sourceFile = do
  inferredPkg <- snd <$> inferFile sourceFile
  validateTypedPkg inferredPkg
  return inferredPkg


resolveImplsInPackage :: TypedPackage -> CLI (CompileEnvironment, TypedPackage)
resolveImplsInPackage inferredPkg = do
  resolvedPkg <- resolvePkg (environmentFromPackage inferredPkg) inferredPkg
  return (environmentFromPackage resolvedPkg, resolvedPkg)


compileFile :: SourceFile -> CLI MonomorphedPackage
compileFile sourceFile = do
  inferredPkg <- inferAndValidateFile sourceFile
  (compileEnv, resolvedPkg) <- resolveImplsInPackage inferredPkg
  liftEither (compile compileEnv resolvedPkg)


codegenPkg :: MonomorphedPackage -> CLI [CompiledFile]
codegenPkg compiledPkg = do
  opts <- ask
  liftEither (codegen (GoBackend $ outPath opts) compiledPkg)


build :: CLI ()
build = do
  opts <- ask
  sourceFiles <- liftIO $ scan (odenPath opts)
  pkgs <- mapM compileFile sourceFiles
  outFiles <- mapM codegenPkg pkgs
  mapM_ writeCompiledFile (concat outFiles)
  logCompiledFiles (concat outFiles)
