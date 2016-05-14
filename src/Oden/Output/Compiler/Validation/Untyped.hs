module Oden.Output.Compiler.Validation.Untyped where

import           Text.PrettyPrint.Leijen

import           Oden.Compiler.Validation.Untyped
import           Oden.Output
import           Oden.Pretty ()

instance OdenOutput ValidationError where
  outputType _ = Error

  name Redefinition{}              = "Compiler.Validation.Redefinition"
  name DuplicatedRecordFieldName{} = "Compiler.Validation.DuplicatedRecordFieldName"

  header (Redefinition _ i) s =
    code s (pretty i) <+> text "is already defined"
  header (DuplicatedRecordFieldName _ n) _ =
    text "Duplicate struct field name" <+> pretty n


  details Redefinition{} _              = text "Shadowing is not allowed"
  details DuplicatedRecordFieldName{} _ = empty

  sourceInfo (Redefinition si _)              = Just si
  sourceInfo (DuplicatedRecordFieldName si _) = Just si

instance OdenOutput ValidationWarning where
  outputType _ = Warning

  name _     = "Compiler.Validation.Warning"

  header _ _ = empty

  details _ _ = empty

  sourceInfo _ = Nothing
