{-# LANGUAGE LambdaCase #-}
module Oden.Output.Explode where

import           Text.PrettyPrint.Leijen

import           Oden.Explode
import           Oden.Output
import           Oden.Pretty             ()
import           Oden.Type.Signature

instance OdenOutput ExplodeError where
  outputType _ = Error

  name = \case
    TypeSignatureWithoutDefinition{} -> "Explode.TypeSignatureWithoutDefinition"
    TypeSignatureRedefinition{}      -> "Explode.TypeSignatureRedefinition"
    InvalidMemberAccessExpression{}  -> "Explode.InvalidMemberAccessExpression"
    InvalidProtocolMethodReference{} -> "Explode.InvalidProtocolMethodReference"

  header expr s = case expr of
    TypeSignatureWithoutDefinition{} ->
      text "Type signature has no corresponding definition"
    TypeSignatureRedefinition{} ->
      text "Redundant type signature definition"
    InvalidMemberAccessExpression _ _ nonName ->
      text "Invalid member access expression" <+> code s (pretty nonName)
    InvalidProtocolMethodReference{} ->
      text "Invalid protocol method reference"

  details expr s = case expr of
    TypeSignatureWithoutDefinition _ n ts@(TypeSignature _ _ TSFn{}) ->
      text "Define the function" <+> code s (pretty n)
      <+> text "with type" <+> code s (pretty ts)
    TypeSignatureRedefinition _ n (Just ts) ->
      text "Duplicate type signature for" <+> code s (pretty n)
      <+> text ". Already defined as" <+> code s (pretty ts)
    TypeSignatureRedefinition _ n Nothing ->
      text "Type signature must precede the definition for"
      <+> code s (pretty n)
    InvalidMemberAccessExpression _ expr' _ ->
      text "In the expression" <+> code s (pretty expr')
    InvalidProtocolMethodReference _ protocol method ->
      text "In the expression" <+> code s (pretty protocol <> text "::" <> pretty method)
    TypeSignatureWithoutDefinition _ n sc ->
      text "Define the value" <+> code s (pretty n)
      <+> text "with type" <+> code s (pretty sc)

  sourceInfo = \case
    TypeSignatureWithoutDefinition si _ _ -> Just si
    TypeSignatureRedefinition si _ _      -> Just si
    InvalidMemberAccessExpression si _ _  -> Just si
    InvalidProtocolMethodReference si _ _ -> Just si
