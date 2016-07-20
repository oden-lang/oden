module Oden.CLI.PrintPackage where

import           Oden.Backend
import           Oden.QualifiedName
import           Oden.Pretty             ()
import           Oden.SourceFile

import           Oden.CLI
import           Oden.CLI.Build

import           Oden.Core.Definition
import           Oden.Core.Typed

import           Control.Monad.Reader

import           Text.PrettyPrint.Leijen hiding ((<$>))

render :: Doc -> String
render doc = displayS (renderPretty 0.4 120 doc) ""

mainPkg :: PackageName
mainPkg = NativePackageName ["main"]

printInferred :: FilePath -> CLI ()
printInferred path = do
  (_, pkg) <- inferFile (OdenSourceFile path mainPkg)
  liftIO $ putStrLn $ render $ pretty pkg

printTypes :: FilePath -> CLI ()
printTypes path = do
  (_, TypedPackage _ _ definitions) <- inferFile (OdenSourceFile path mainPkg)
  liftIO $ putStrLn $ render $ vcat $ map prettyScheme definitions
  where
  prettyScheme (Definition _ name (scheme, _)) = pretty name <+> text ":" <+> pretty scheme
  prettyScheme _ = empty

printEnv :: FilePath -> CLI ()
printEnv path = do
  env <- fst <$> inferFile (OdenSourceFile path mainPkg)
  liftIO $ putStrLn $ render $ pretty env

printResolved :: FilePath -> CLI ()
printResolved path = do
  typedPkg <- inferAndValidateFile (OdenSourceFile path mainPkg)
  pkg <- snd <$> resolveImplsInPackage typedPkg
  liftIO $ putStrLn $ render $ pretty pkg

printCompiled :: FilePath -> CLI ()
printCompiled path = do
  pkg <- compileFile (OdenSourceFile path mainPkg)
  liftIO $ putStrLn $ render $ pretty pkg

printCodeGen :: FilePath -> CLI ()
printCodeGen path = do
  pkg <- compileFile (OdenSourceFile path mainPkg)
  files <- codegenPkg pkg
  mapM_ printFile files
  where
    printFile (CompiledFile _ s) = liftIO $ putStrLn s
