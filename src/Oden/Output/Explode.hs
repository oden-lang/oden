module Oden.Output.Explode where

import           Text.PrettyPrint.Leijen

import           Oden.Explode
import           Oden.Output
import           Oden.Pretty             ()
import           Oden.Type.Signature

instance OdenOutput ExplodeError where
  outputType _ = Error
  name TypeSignatureWithoutDefinition{} =
    "Explode.TypeSignatureWithoutDefinition"
  name TypeSignatureRedefinition{} =
    "Explode.TypeSignatureRedefinition"
  name InvalidMemberAccessExpression{} =
    "Explode.InvalidMemberAccessExpression"

  header TypeSignatureWithoutDefinition{} _ =
    text "Type signature has no corresponding definition"
  header TypeSignatureRedefinition{} _ =
    text "Redundant type signature definition"
  header (InvalidMemberAccessExpression _ _ nonName) s =
    text "Invalid member access expression" <+> code s (pretty nonName)

  details (TypeSignatureWithoutDefinition _ n ts@(TypeSignature _ _ TSFn{})) s =
    text "Define the function" <+> code s (pretty n)
    <+> text "with type" <+> code s (pretty ts)
  details (TypeSignatureRedefinition _ n ts) s
    = case ts of
        Just ts' -> text "Duplicate type signature for" <+> code s (pretty n)
                    <+> text ". Already defined as" <+> code s (pretty ts')
        Nothing -> text "Type signature must precede the definition for"
                   <+> code s (pretty n)
  details (InvalidMemberAccessExpression _ expr' _) s =
    text "In the expression" <+> code s (pretty expr')

  details (TypeSignatureWithoutDefinition _ n sc) s =
    text "Define the value" <+> code s (pretty n)
    <+> text "with type" <+> code s (pretty sc)

  sourceInfo (TypeSignatureWithoutDefinition si _ _) = Just si
  sourceInfo (TypeSignatureRedefinition si _ _)      = Just si
  sourceInfo (InvalidMemberAccessExpression si _ _)  = Just si
