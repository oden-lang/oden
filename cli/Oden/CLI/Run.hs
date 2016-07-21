module Oden.CLI.Run where

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.QualifiedName
import           Oden.SourceFile

import           Control.Monad.Reader

import           System.Directory
import           System.FilePath
import           System.Process
import           System.IO.Temp

import           Oden.CLI
import           Oden.CLI.Build

run :: FilePath -> CLI ()
run path = do
  pkg <- compileFile (OdenSourceFile path (NativePackageName ["main"]))
  tmpDir <- liftIO getTemporaryDirectory
  tmp <- liftIO (createTempDirectory tmpDir "oden-run.go")
  files <- liftEither (codegen (GoBackend tmp) pkg)
  mapM_ writeCompiledFile files
  case filter isMainPackage files of
    [] -> liftIO $ exitWithMessage "Not a main package!"
    [CompiledFile name _] -> liftIO $ callCommand ("go run " ++ name)
    _ -> liftIO $ exitWithMessage "Cannot run with multiple main packages!"
  where
  isMainPackage :: CompiledFile -> Bool
  isMainPackage (CompiledFile name _) = takeBaseName name == "main"
