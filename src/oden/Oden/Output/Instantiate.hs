module Oden.Output.Instantiate where

import Text.PrettyPrint

import Oden.Output
import Oden.Compiler.Instantiate

instance OdenOutput InstantiateError where
  outputType _ = Error
  name (TypeMismatch _ _)       = "Instantiate.TypeMismatch"
  name (SubstitutionFailed _ _) = "Instantiate.SubstitutionFailed"

  header (TypeMismatch pt mt) _         = text "Type mismatch in instantiation"
  header (SubstitutionFailed tvar _) s  = text "Substitution failed for type variable " <+> code s tvar

  details (TypeMismatch pt mt) s        = text "Polymorphic type" <+> code s pt <+> text "cannot be instantiated to" <+> code s mt
  details (SubstitutionFailed _ vars) s = text "Type variables in context:" <+> hcat (map (code s) vars)

