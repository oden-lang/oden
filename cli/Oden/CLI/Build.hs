module Oden.CLI.Build where

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.Compiler
import           Oden.Compiler.Monomorphization  (MonomorphedPackage(..))
import           Oden.Compiler.Validation
import qualified Oden.Core                       as Core
import qualified Oden.Core.Untyped               as Untyped
import qualified Oden.Env                        as Env
import           Oden.Explode
import qualified Oden.Go                         as Go
import           Oden.Infer
import           Oden.Parser
import           Oden.Predefined
import           Oden.Scanner
import qualified Oden.Scope                      as Scope
import qualified Oden.Syntax                     as Syntax

import           Control.Monad.Reader
import qualified Data.Text.Lazy.IO               as L
import           System.Directory
import           System.FilePath

import Oden.CLI

writeCompiledFile :: CompiledFile -> CLI ()
writeCompiledFile (CompiledFile name contents) =
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory name)
    writeFile name contents

readPackage :: FilePath -> CLI Syntax.Package
readPackage fname = do
  contents <- liftIO $ L.readFile fname
  liftEither $ parsePackage fname contents

validatePkg :: Core.Package -> CLI ()
validatePkg pkg = do
  warnings <- liftEither (validate pkg)
  mapM_ logWarning warnings

logCompiledFiles :: [CompiledFile] -> CLI ()
logCompiledFiles [_] = liftIO $ putStrLn "Compiled 1 Go source file."
logCompiledFiles files = liftIO $ putStrLn $ "Compiled " ++ show (length files) ++ " Go source files."


scanImports :: Untyped.Package -> CLI Scope.Scope
scanImports (Untyped.Package _ imports _) = foldM scanImport Scope.empty imports
  where scanImport scope' (Untyped.Import _ pn) = do
          (pkgScope, warning) <- liftIO (Go.getPackageScope pn) >>= liftEither
          case warning of
            Just w -> logWarning w
            _ -> return ()
          return (Scope.merge scope' pkgScope)

compileFile :: SourceFile -> CLI MonomorphedPackage
compileFile (OdenSourceFile fname _) = do
  -- TODO: Check package name
  syntaxPkg <- readPackage fname
  corePkg <- liftEither' (explodePackage syntaxPkg)
  importsScope <- scanImports corePkg
  let scope' = Scope.merge predefined importsScope
      typeEnv = Env.fromScope scope'
  (inferredPkg, _) <- liftEither (inferPackage typeEnv corePkg)
  validatePkg inferredPkg
  liftEither (compile scope' inferredPkg)

codegenPkg :: MonomorphedPackage -> CLI [CompiledFile]
codegenPkg compiledPkg = do
  opts <- ask
  liftEither (codegen (GoBackend $ outPath opts) compiledPkg)

build :: CLI ()
build = do
  opts <- ask
  sourceFiles <- liftIO $ scan (odenPath opts </> "src")
  pkgs <- mapM compileFile sourceFiles
  outFiles <- mapM codegenPkg pkgs
  mapM_ writeCompiledFile (concat outFiles)
  logCompiledFiles (concat outFiles)
