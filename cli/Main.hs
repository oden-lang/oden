{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Oden.CLI
import Oden.CLI.Build
import Oden.CLI.Run
import Oden.CLI.Lint

import           Control.Monad.Reader
import           Control.Monad.Except

-- versioning stuff
import qualified Data.Version                    as Version
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
    Right _ -> putStrLn help
