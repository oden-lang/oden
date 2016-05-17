{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Oden.Output.Compiler.Resolution where

import           Text.PrettyPrint.Leijen

import           Oden.Compiler.Resolution
import           Oden.Core.ProtocolImplementation
import           Oden.Metadata
import           Oden.Output
import           Oden.Pretty                      ()

instance OdenOutput ResolutionError where
  outputType _ = Error

  name =
    \case
      NoMatchingImplementationInScope{} ->
        "Compiler.Resolution.NoMatchingImplementationInScope"
      MultipleMatchingImplementationsInScope{} ->
        "Compiler.Resolution.MultipleMatchingImplementationsInScope"

  header e _ =
    case e of
      NoMatchingImplementationInScope _ protocol type' _ ->
        text "No matching implementation in scope for"
        <+> pretty protocol <+> parens (pretty type')
      MultipleMatchingImplementationsInScope{} ->
        text "Multiple matching implementations in scope"

  details e _s =
    case e of
      NoMatchingImplementationInScope _ _ _ allImpls ->
        vcat (text "The following implementations are in scope:" : map pretty allImpls)
      MultipleMatchingImplementationsInScope _ impls ->
        vcat (text "The following implementations matched:" : concatMap printImpl impls)
        where
        printImpl impl@(ProtocolImplementation (Metadata si) _ _ _) =
          [ empty
          , pretty impl
          , text "defined at" <+> pretty si
          , empty
          ]


  sourceInfo =
    \case
      NoMatchingImplementationInScope si _ _ _    -> Just si
      MultipleMatchingImplementationsInScope si _ -> Just si
