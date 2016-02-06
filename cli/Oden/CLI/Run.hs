module Oden.CLI.Run where

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.Scanner

import           Control.Monad.Reader

import           System.Directory
import           System.FilePath
import           System.Process
import           System.IO.Temp

import           Oden.CLI
import           Oden.CLI.Build

run :: FilePath -> CLI ()
run path = do
  pkg <- compileFile (OdenSourceFile path ["main"])
  tmpDir <- liftIO getTemporaryDirectory
  tmp <- liftIO (createTempDirectory tmpDir "oden-run.go")
  files <- liftEither (codegen (GoBackend tmp) pkg)
  mapM_ writeCompiledFile files
  liftIO $ callCommand ("go run " ++ (tmp </> "src" </> "main.go"))
