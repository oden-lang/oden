module Oden.Output.Compiler.Validation where

import           Text.PrettyPrint

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Output

instance OdenOutput ValidationError where
  outputType _ = Error

  name (Redefinition _)     = "Compiler.Validation.Redefinition"
  name (UnitDefinition _)   = "Compiler.Validation.UnitDefinition"
  name (UnitBinding _ _)    = "Compiler.Validation.UnitBinding"
  name (UnitFnArg _ _)      = "Compiler.Validation.UnitFnArg"

  header (Redefinition i) s = strCode s i <+> text "is already defined"
  header (UnitDefinition (Definition n _)) s =
    text "Cannot define" <+>
    strCode s n <+>
    text "with type" <+>
    strCode s "{}"
  header (UnitBinding n _) s =
    text "Cannot bind" <+>
    strCode s n <+>
    text "with type" <+>
    strCode s "{}"
  header (UnitFnArg n _) s =
    text "Cannot accept function argument" <+>
    strCode s n <+>
    text "with type" <+>
    strCode s "{}"

  details (Redefinition _) _ = text "Shadowing is not allowed"
  details (UnitDefinition (Definition n (sc, te))) _ =
    text "In the expression:" $+$
    text (n ++ " :: " ++ show sc) $+$
    text (n ++ " = " ++ show te)

  details (UnitBinding _ e) s =
    text "In the expression:" <+>
    code s e
  details (UnitFnArg _ e) s =
    text "In the expression:" <+>
    code s e

instance OdenOutput ValidationWarning where
  outputType (ValueDiscarded _)   = Warning

  name (ValueDiscarded _)         = "Compiler.Validation.ValueDiscarded"

  header (ValueDiscarded e) s     = text "Value of type"
                                    <+> code s (typeOf e)
                                    <+> text "discarded"

  details (ValueDiscarded _) _    = empty

