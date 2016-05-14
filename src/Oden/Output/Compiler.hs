{-# LANGUAGE LambdaCase #-}
module Oden.Output.Compiler where

import           Text.PrettyPrint.Leijen

import           Oden.Compiler
import           Oden.Compiler.Monomorphization
import           Oden.Output
import           Oden.Output.Instantiate        ()
import           Oden.Pretty                    ()

instance OdenOutput MonomorphError where
  outputType _ = Error

  name =
    \case
      NotInScope{}                  -> "Compiler.Monomorph.NotInScope"
      UnexpectedPolyType{}          -> "Compiler.Monomorph.UnexpectedPolyType"
      MonomorphInstantiateError err -> name err
      UnresolvedMethodReference{}   -> "Compiler.Monomorph.UnresolvedMethodReference"

  header err s =
    case err of
      NotInScope i ->
        code s (pretty i) <+> text "is not in scope"
      UnexpectedPolyType _ e ->
        text "Unexpected polymorphic type" <+> code s (pretty e)
      MonomorphInstantiateError nestedError ->
        header nestedError s
      UnresolvedMethodReference _ _ _ constraint ->
        text "Unresolved method reference for constraint" <+> code s (pretty constraint)

  details err s =
    case err of
      MonomorphInstantiateError e -> details e s
      NotInScope _                -> empty
      UnexpectedPolyType _ _      ->
        text "This can usually be fixed by adding (stricter) type signatures to top-level forms."
      UnresolvedMethodReference _ protocol method _ ->
        vcat [ text "Caused by the use of" <+> code s (pretty protocol <> text "::" <> pretty method)
             , text "This error should have been caught in the resolution phase and should be considered a compiler bug."
             ]

  sourceInfo =
    \case
      NotInScope{}                       -> Nothing
      UnexpectedPolyType si _            -> Just si
      MonomorphInstantiateError e        -> sourceInfo e
      UnresolvedMethodReference si _ _ _ -> Just si

instance OdenOutput CompilationError where
  outputType (MonomorphError e) = outputType e
  name (MonomorphError e)       = name e
  header (MonomorphError e)     = header e
  details (MonomorphError e)    = details e
  sourceInfo (MonomorphError e) = sourceInfo e
