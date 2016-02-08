module Oden.Output.Compiler.Validation where

import           Text.PrettyPrint

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Output

instance OdenOutput ValidationError where
  outputType _ = Error

  name (Redefinition _)     = "Compiler.Validation.Redefinition"

  header (Redefinition i) s = strCode s i <+> text "is already defined"

  details (Redefinition _) _ = text "Shadowing is not allowed"

  sourceInfo _ = Nothing

instance OdenOutput ValidationWarning where
  outputType (ValueDiscarded _)   = Warning

  name (ValueDiscarded _)         = "Compiler.Validation.ValueDiscarded"

  header (ValueDiscarded e) s     = text "Value of type"
                                    <+> code s (typeOf e)
                                    <+> text "discarded"

  details (ValueDiscarded _) _    = empty

  sourceInfo _ = Nothing
