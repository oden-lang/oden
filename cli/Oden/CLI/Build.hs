module Oden.CLI.Build where

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.Compiler
import           Oden.Compiler.Monomorphization  (MonomorphedPackage(..))
import           Oden.Compiler.Validation
import           Oden.Compiler.Environment       as CE
import qualified Oden.Core                       as Core
import qualified Oden.Core.Untyped               as Untyped
import           Oden.Explode
import qualified Oden.Go                         as Go
import           Oden.Infer
import qualified Oden.Infer.Environment          as IE
import           Oden.Parser
import           Oden.Predefined
import           Oden.Scanner
import           Oden.SourceInfo
import qualified Oden.Environment                as Environment
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

compileToTypeBinding :: CE.Binding -> IE.TypeBinding
compileToTypeBinding (CE.Package si n p) = IE.Package si n (Environment.map compileToTypeBinding p)
compileToTypeBinding (CE.Definition (Core.Definition si n (sc, _))) = Local si n sc
compileToTypeBinding (CE.Definition (Core.ForeignDefinition si n sc)) = Local si n sc
compileToTypeBinding (CE.Definition (Core.ForeignDefinition si n sc)) = Local si n sc

scanImports :: Untyped.Package -> CLI CompileEnvironment
scanImports (Untyped.Package _ imports _) = foldM scanImport Environment.empty imports
  where
  scanImport :: CompileEnvironment -> Untyped.Import -> CLI CompileEnvironment
  scanImport env (Untyped.Import _ pn) = do
    (defs, warning) <- liftIO (Go.getPackageDefinitions pn) >>= liftEither
    case warning of
      Just w -> logWarning w
      _ -> return ()
    let n = last pn
    return (env `Environment.extend` (n, CE.Package Missing n (CE.fromDefinitions defs)))

compileFile :: SourceFile -> CLI MonomorphedPackage
compileFile (OdenSourceFile fname _) = do
  -- TODO: Check package name
  syntaxPkg <- readPackage fname
  corePkg <- liftEither' (explodePackage syntaxPkg)
  importsEnvironment <- scanImports corePkg
  let env' = fromDefinitions predefined `Environment.merge` importsEnvironment
      typeEnv = Environment.map compileToTypeBinding env'
  (inferredPkg, _) <- liftEither (inferPackage typeEnv corePkg)
  validatePkg inferredPkg
  liftEither (compile env' inferredPkg)

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
