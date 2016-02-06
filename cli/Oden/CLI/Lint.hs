module Oden.CLI.Lint where

import           Oden.Scanner

import           Oden.CLI
import           Oden.CLI.Build

lint :: FilePath -> CLI ()
lint path = do
  _ <- compileFile (OdenSourceFile path ["main"])
  return ()
