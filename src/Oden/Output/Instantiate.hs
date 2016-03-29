module Oden.Output.Instantiate where

import Text.PrettyPrint.Leijen

import Oden.Compiler.Instantiate
import Oden.Output
import Oden.Pretty ()

instance OdenOutput InstantiateError where
  outputType _ = Error

  name TypeMismatch{}       = "Instantiate.TypeMismatch"
  name SubstitutionFailed{} = "Instantiate.SubstitutionFailed"

  header TypeMismatch{} _ =
    text "Type mismatch in instantiation"
  header (SubstitutionFailed _ tvar _) s =
    text "Substitution failed for type variable " <+> code s (pretty tvar)

  details (TypeMismatch _ pt mt) s =
    text "Polymorphic type" <+> code s (pretty pt)
    <+> text "cannot be instantiated to" <+> code s (pretty mt)
  details (SubstitutionFailed _ _ vars) s =
    text "Type variables in context:" <+> hcat (map (code s . pretty) vars)

  sourceInfo (TypeMismatch si _ _)       = Just si
  sourceInfo (SubstitutionFailed si _ _) = Just si
