module Oden.Output.Explode where

import Text.PrettyPrint

import Oden.Output
import Oden.Explode
import Oden.Type.Polymorphic
import Oden.Pretty

instance OdenOutput ExplodeError where
  outputType _ = Error
  name TypeSignatureWithoutDefinition{} =
    "Explode.TypeSignatureWithoutDefinition"

  header TypeSignatureWithoutDefinition{} _ =
    text "Type signature has no corresponding definition"

  details (TypeSignatureWithoutDefinition _ n sc@(Forall _ _ TFn{})) s =
    text "Define the function" <+> strCode s n
    <+> text "with type" <+> code s (pp sc)

  details (TypeSignatureWithoutDefinition _ n sc) s =
    text "Define the value" <+> strCode s n
    <+> text "with type" <+> code s (pp sc)

  sourceInfo (TypeSignatureWithoutDefinition si _ _) = Just si
