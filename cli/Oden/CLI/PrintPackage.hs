module Oden.CLI.PrintPackage where

import           Oden.Core
import           Oden.Compiler.Monomorphization
import           Oden.Metadata
import           Oden.Pretty
import           Oden.Scanner

import           Oden.CLI
import           Oden.CLI.Build

import           Control.Monad.Reader

import           Data.Set                       (toList)
import           Text.PrettyPrint

printInstance :: InstantiatedDefinition -> Doc
printInstance (InstantiatedDefinition polyName (Metadata si) name expr) =
  text "//" <+> pp polyName $+$
  text "//" <+> text (show si) $+$
  pp name <+> text "::" <+> pp (typeOf expr) $+$
  pp name <+> text "=" <+> pp expr

printMonomorphed :: MonomorphedDefinition -> Doc
printMonomorphed (MonomorphedDefinition _ name type' expr) =
  pp name <+> text "::" <+> pp type' $+$ pp name <+> text "=" <+> pp expr

printPackage :: FilePath -> CLI ()
printPackage path = do
  (MonomorphedPackage _ _ is ms) <- compileFile (OdenSourceFile path ["main"])
  liftIO $ do
    putStrLn "// INSTANCES\n"
    putStrLn (render $ vcat $ map printInstance $ toList is)
    putStrLn ""
    putStrLn "// MONOMORPHED\n"
    putStrLn (render $ vcat $ map printMonomorphed $ toList ms)
