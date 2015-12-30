module Oden.Output.Compiler where

import           Text.PrettyPrint

import           Oden.Compiler
import           Oden.Output
import           Oden.Output.Instantiate

instance OdenOutput CompilationError where
  outputType _ = Error
  name (NotInScope _)                   = "Compiler.NotInScope"
  name (AmbigiousReference _ _)         = "Compiler.AmbigiousReference"
  name (UnexpectedPolyType _)           = "Compiler.UnexpectedPolyType"
  name (MonomorphInstantiateError err)  = name err

  header (NotInScope i) = code i <+> text "is not in scope"
  header (AmbigiousReference i _) = text "Ambigious reference" <+> code i
  header (UnexpectedPolyType e) = text "Unexpected polymorphic type" <+> code e
  header (MonomorphInstantiateError err) = header err

  details (NotInScope i) = empty
  details _ = empty

