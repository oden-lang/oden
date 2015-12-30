module Oden.Output.Instantiate where

import Text.PrettyPrint

import Oden.Output
import Oden.Compiler.Instantiate

instance OdenOutput InstantiateError where
  outputType _ = Error
  name (TypeMismatch _ _)       = "Instantiate.TypeMismatch"
  name (SubstitutionFailed _ _) = "Instantiate.SubstitutionFailed"

  header (TypeMismatch pt mt) = text "Type mismatch in instantiation"
  header (SubstitutionFailed tvar _) = text "Substitution failed for type variable " <+> code tvar

  details (TypeMismatch pt mt) = text "Polymorphic type" <+> code pt <+> text "cannot be instantiated to" <+> code mt
  details (SubstitutionFailed _ vars) = text "Type variables in context:" <+> hcat (map code vars)

