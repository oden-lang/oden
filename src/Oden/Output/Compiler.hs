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

  header (NotInScope i) s                   = code s i <+> text "is not in scope"
  header (AmbigiousReference i _) s         = text "Ambigious reference" <+> code s i
  header (UnexpectedPolyType e) s           = text "Unexpected polymorphic type" <+> code s e
  header (MonomorphInstantiateError err) s  = header err s

  details (NotInScope i) _  = empty
  details _ _               = empty

