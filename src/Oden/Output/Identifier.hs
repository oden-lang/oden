{-# LANGUAGE LambdaCase #-}
module Oden.Output.Identifier where

import Text.PrettyPrint.Leijen

import Oden.Identifier
import Oden.Output

instance OdenOutput IdentifierValidationError where
  outputType _ = Error

  name =
    \case
      Empty ->
        "Identifier.ValidationError.Empty"
      IllegalStart{} ->
        "Identifier.ValidationError.IllegalStart"
      IllegalCharacters{} ->
        "Identifier.ValidationError.IllegalCharacters"

  header err s =
    case err of
      Empty ->
        text "Identifiers cannot be empty"
      IllegalStart c ->
        text "Identifier cannot start with:" <+> strCode s [c]
      IllegalCharacters cs ->
        text "Identifier cannot contain any of the characters:" <+> strCode s cs

  -- TODO: Print helpful explanation of what rules apply to identifiers.
  details _ _ = empty

  sourceInfo _ = Nothing
