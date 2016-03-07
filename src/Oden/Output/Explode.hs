module Oden.Output.Explode where

import Text.PrettyPrint

import Oden.Output
import Oden.Explode
import Oden.Type.Signature
import Oden.Pretty

instance OdenOutput ExplodeError where
  outputType _ = Error
  name TypeSignatureWithoutDefinition{} =
    "Explode.TypeSignatureWithoutDefinition"
  name InvalidMemberAccessExpression{} =
    "Explode.InvalidMemberAccessExpression"

  header TypeSignatureWithoutDefinition{} _ =
    text "Type signature has no corresponding definition"
  header (InvalidMemberAccessExpression _ _ nonName) s =
    text "Invalid member access expression" <+> code s (pp nonName)

  details (TypeSignatureWithoutDefinition _ n sc@(Explicit _ _ TSFn{})) s =
    text "Define the function" <+> code s (pp n)
    <+> text "with type" <+> code s (pp sc)
  details (InvalidMemberAccessExpression _ expr' _) s =
    text "In the expression" <+> code s (pp expr')

  details (TypeSignatureWithoutDefinition _ n sc) s =
    text "Define the value" <+> code s (pp n)
    <+> text "with type" <+> code s (pp sc)

  sourceInfo (TypeSignatureWithoutDefinition si _ _) = Just si
  sourceInfo (InvalidMemberAccessExpression si _ _) = Just si
