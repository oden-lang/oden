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
  name DivisionByZero{}            = "Compiler.Validation.DivisionByZero"
  name NegativeSliceIndex{}        = "Compiler.Validation.NegativeSliceIndex"
  name InvalidSubslice{}         = "Compiler.Validation.InvalidSubslice"

  header (Redefinition _ i) s =
    code s (pretty i) <+> text "is already defined"
  header (ValueDiscarded e) s =
    text "Value of type"
    <+> code s (pretty (typeOf e))
    <+> text "discarded"
  header (DuplicatedRecordFieldName _ n) _ =
    text "Duplicate struct field name" <+> pretty n
  header (DivisionByZero e) s =
    text "Division by zero: "
    <+> code s (pretty e)
  header (NegativeSliceIndex e) s =
    text "Negative index: "
    <+> code s (pretty e)
  header (InvalidSubslice _ r) s =
    text "Invalid subslice range "
    <+> code s (pretty r)


  details Redefinition{} _              = text "Shadowing is not allowed"
  details ValueDiscarded{} _            = empty
  details DuplicatedRecordFieldName{} _ = empty
  details DivisionByZero{} _            = empty
  details NegativeSliceIndex{} _        = text "Indices must be >= 0"
  details InvalidSubslice{} _           = text "Ranges must go from smaller to bigger"

  sourceInfo (Redefinition si _) = Just si
  sourceInfo (ValueDiscarded e) = Just (getSourceInfo e)
  sourceInfo (DuplicatedRecordFieldName si _) = Just si
  sourceInfo (DivisionByZero e)  = Just (getSourceInfo e)
  sourceInfo (NegativeSliceIndex e) = Just (getSourceInfo e)
  sourceInfo (InvalidSubslice si _) = Just si

instance OdenOutput ValidationWarning where
  outputType _ = Warning

  name _     = "Compiler.Validation.Warning"

  header _ _ = empty

  details _ _ = empty

  sourceInfo _ = Nothing
