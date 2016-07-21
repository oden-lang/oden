{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Oden.CLI
import           Oden.CLI.Build
import           Oden.CLI.Lint
import           Oden.CLI.PrintPackage
import           Oden.CLI.Run

import           Control.Monad.Except
import           Control.Monad.Reader

-- versioning stuff
import qualified Data.Version          as Version
import           Paths_oden

exec :: Options -> CLI () -> IO ()
exec opts m = do
  result <- runExceptT (runReaderT m opts)
  case result of
    Left err -> exitWithMessage err
    Right _ -> return ()

main :: IO ()
main = do
  o <- getOptions
  case o of
    Left err -> exitWithMessage err
    Right (opts, _) | showHelp opts -> putStrLn help
    Right (opts, _) | showVersion opts -> putStrLn (Version.showVersion version)
    Right (opts, ["build"]) ->  exec opts build
    Right (opts, ["run", path]) -> exec opts (run path)
    Right (opts, ["lint", path]) -> exec opts (lint path)
    Right (opts, ["print-inferred", path]) -> exec opts (printInferred path)
    Right (opts, ["print-resolved", path]) -> exec opts (printResolved path)
    Right (opts, ["print-env", path]) -> exec opts (printEnv path)
    Right (opts, ["print-types", path]) -> exec opts (printTypes path)
    Right (opts, ["print-compiled", path]) -> exec opts (printCompiled path)
    Right (opts, ["print-codegen", path]) -> exec opts (printCodeGen path)
    Right _ -> putStrLn help
