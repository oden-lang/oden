module Oden.Output.Explode where

import Text.PrettyPrint

import Oden.Output
import Oden.Explode
import Oden.Type.Polymorphic

instance OdenOutput ExplodeError where
  outputType _ = Error
  name (TypeSignatureWithoutDefinition _ _) =
    "Explode.TypeSignatureWithoutDefinition"

  header (TypeSignatureWithoutDefinition _ _) _ =
    text "Type signature has no corresponding definition"

  details (TypeSignatureWithoutDefinition n sc@(Forall _ TFn{})) s =
    text "Define the function" <+> strCode s n
    <+> text "with type" <+> code s sc

  details (TypeSignatureWithoutDefinition n sc) s =
    text "Define the value" <+> strCode s n
    <+> text "with type" <+> code s sc
