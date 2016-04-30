module Oden.CLI.PrintPackage where

import           Oden.Pretty ()
import           Oden.Scanner

import           Oden.CLI
import           Oden.CLI.Build

import           Oden.Core.Definition
import           Oden.Core.Typed

import qualified Oden.Environment     as Env

import           Control.Monad.Reader

import           Text.PrettyPrint.Leijen

render :: Doc -> String
render doc = displayS (renderPretty 0.4 120 doc) ""

printInferred :: FilePath -> CLI ()
printInferred path = do
  (_, pkg) <- inferFile (OdenSourceFile path ["main"])
  liftIO $ putStrLn $ render $ pretty pkg

printTypes :: FilePath -> CLI ()
printTypes path = do
  (_, TypedPackage _ _ definitions) <- inferFile (OdenSourceFile path ["main"])
  liftIO $ putStrLn $ render $ vcat $ map prettyScheme definitions
  where
  prettyScheme (Definition _ name (scheme, _)) = pretty name <+> text ":" <+> pretty scheme
  prettyScheme _ = empty

printEnv :: FilePath -> CLI ()
printEnv path = do
  (env, _) <- inferFile (OdenSourceFile path ["main"])
  liftIO $ putStrLn $ render $ pretty env

printCompiled :: FilePath -> CLI ()
printCompiled path = do
  pkg <- compileFile (OdenSourceFile path ["main"])
  liftIO $ putStrLn $ render $ pretty pkg
