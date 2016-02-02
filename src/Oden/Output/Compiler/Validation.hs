module Oden.Output.Compiler.Validation where

import           Text.PrettyPrint

import           Oden.Compiler.Validation
import           Oden.Output

instance OdenOutput ValidationError where
  outputType _ = Error

  name (Redefinition _) = "Compiler.Validation.Redefinition"

  header (Redefinition i) s = strCode s i <+> text "is already defined"

  details (Redefinition _) _ = empty


instance OdenOutput ValidationWarning where
  outputType (NameShadowed _)   = Warning

  name (NameShadowed _)         = "Compiler.Validation.NameShadowed"

  header (NameShadowed n) s     = text "Binding shadows" <+> strCode s n

  details (NameShadowed _) _    = empty

