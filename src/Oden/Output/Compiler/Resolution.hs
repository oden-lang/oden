{-# LANGUAGE LambdaCase #-}
module Oden.Output.Compiler.Resolution where

import           Text.PrettyPrint.Leijen

import           Oden.Compiler.Resolution
import           Oden.Output
import           Oden.Pretty                    ()

instance OdenOutput ResolutionError where
  outputType _ = Error

  name = \case
    NoMatchingImplementationInScope{}          ->
      "Compiler.Resolution.NoMatchingImplementationInScope"
    MultipleMatchingImplementationsInScope _ _ ->
      "Compiler.Resolution.MultipleMatchingImplementationsInScope"

  header e _s = case e of
    NoMatchingImplementationInScope _ protocol method type' _ ->
      text "No matching implementation in scope for"
      <+> pretty protocol <> text "::" <> pretty method
      <+> text "with type" <+> pretty type'
    MultipleMatchingImplementationsInScope{} ->
      text "Multiple matching implementations in scope"

  details e _s = case e of
    NoMatchingImplementationInScope _ _ _ _ allImpls ->
      vcat (text "The following implementations are in scope:" : map pretty allImpls)
    MultipleMatchingImplementationsInScope _ impls ->
      vcat (text "The following implementations matched:" : map pretty impls)

  sourceInfo = \case
    NoMatchingImplementationInScope si _ _ _ _  -> Just si
    MultipleMatchingImplementationsInScope si _ -> Just si

