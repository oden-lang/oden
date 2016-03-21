module Oden.CLI.PrintPackage where

import           Oden.Pretty
import           Oden.Scanner

import           Oden.CLI
import           Oden.CLI.Build

import           Control.Monad.Reader

import           Text.PrettyPrint

printInferred :: FilePath -> CLI ()
printInferred path = do
  pkg <- inferFile (OdenSourceFile path ["main"])
  liftIO $ putStrLn $ render $ pp pkg

printCompiled :: FilePath -> CLI ()
printCompiled path = do
  pkg <- compileFile (OdenSourceFile path ["main"])
  liftIO $ putStrLn $ render $ pp pkg
