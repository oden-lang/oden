module Oden.Output.Instantiate where

import Text.PrettyPrint

import Oden.Compiler.Instantiate
import Oden.Output
import Oden.Pretty

instance OdenOutput InstantiateError where
  outputType _ = Error
  name TypeMismatch{}       = "Instantiate.TypeMismatch"
  name SubstitutionFailed{} = "Instantiate.SubstitutionFailed"

  header TypeMismatch{} _                = text "Type mismatch in instantiation"
  header (SubstitutionFailed _ tvar _) s = text "Substitution failed for type variable " <+> code s (pp tvar)

  details (TypeMismatch _ pt mt) s        = text "Polymorphic type" <+> code s (pp pt) <+> text "cannot be instantiated to" <+> code s (pp mt)
  details (SubstitutionFailed _ _ vars) s = text "Type variables in context:" <+> hcat (map (code s . pp) vars)

  sourceInfo (TypeMismatch si _ _)       = Just si
  sourceInfo (SubstitutionFailed si _ _) = Just si
