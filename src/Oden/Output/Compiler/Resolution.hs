{-# LANGUAGE LambdaCase #-}
module Oden.Output.Compiler.Resolution where

import           Text.PrettyPrint.Leijen

import           Oden.Compiler.Resolution
import           Oden.Core.ProtocolImplementation
import           Oden.Output
import           Oden.Pretty                    ()
import           Oden.Type.Polymorphic

instance OdenOutput ResolutionError where
  outputType _ = Error

  name = \case
    NoMatchingImplementationInScope _          ->
      "Compiler.Resolution.NoMatchingImplementationInScope"
    MultipleMatchingImplementationsInScope _ _ ->
      "Compiler.Resolution.MultipleMatchingImplementationsInScope"

  header e _s = case e of
    NoMatchingImplementationInScope{}        ->
      text "No matching implementation in scope"
    MultipleMatchingImplementationsInScope{} ->
      text "Multiple matching implementations in scope"

  details e _s = case e of
    NoMatchingImplementationInScope{} -> empty
    MultipleMatchingImplementationsInScope _ impls ->
      vcat (first : map printImpl impls)
      where
      first = text "The following implementations matched:"
      printImpl (ProtocolImplementation _ (Protocol _ protocolName _ _) _) =
        pretty protocolName

  sourceInfo = \case
    NoMatchingImplementationInScope si -> Just si
    MultipleMatchingImplementationsInScope si _ -> Just si

