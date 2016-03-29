module Oden.Output.Compiler.Validation where

import           Text.PrettyPrint.Leijen

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Output
import           Oden.Pretty ()
import           Oden.SourceInfo

instance OdenOutput ValidationError where
  outputType _ = Error

  name Redefinition{}              = "Compiler.Validation.Redefinition"
  name ValueDiscarded{}            = "Compiler.Validation.ValueDiscarded"
  name DuplicatedRecordFieldName{} = "Compiler.Validation.DuplicatedRecordFieldName"

  header (Redefinition _ i) s =
    code s (pretty i) <+> text "is already defined"
  header (ValueDiscarded e) s =
    text "Value of type"
    <+> code s (pretty (typeOf e))
    <+> text "discarded"
  header (DuplicatedRecordFieldName _ n) _ =
    text "Duplicate struct field name" <+> pretty n

  details Redefinition{} _              = text "Shadowing is not allowed"
  details ValueDiscarded{} _            = empty
  details DuplicatedRecordFieldName{} _ = empty

  sourceInfo (Redefinition si _) = Just si
  sourceInfo (ValueDiscarded e) = Just (getSourceInfo e)
  sourceInfo (DuplicatedRecordFieldName si _) = Just si

instance OdenOutput ValidationWarning where
  outputType _ = Warning

  name _     = "Compiler.Validation.Warning"

  header _ _ = empty

  details _ _ = empty

  sourceInfo _ = Nothing
