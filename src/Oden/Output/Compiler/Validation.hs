module Oden.Output.Compiler.Validation where

import           Text.PrettyPrint

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Output
import           Oden.Pretty

instance OdenOutput ValidationError where
  outputType _ = Error

  name (Redefinition _ _)     = "Compiler.Validation.Redefinition"

  header (Redefinition _ i) s = strCode s i <+> text "is already defined"

  details (Redefinition _ _) _ = text "Shadowing is not allowed"

  sourceInfo (Redefinition si _) = Just si

instance OdenOutput ValidationWarning where
  outputType (ValueDiscarded _)   = Warning

  name (ValueDiscarded _)         = "Compiler.Validation.ValueDiscarded"

  header (ValueDiscarded e) s     = text "Value of type"
                                    <+> code s (pp (typeOf e))
                                    <+> text "discarded"

  details (ValueDiscarded _) _    = empty

  sourceInfo _ = Nothing
