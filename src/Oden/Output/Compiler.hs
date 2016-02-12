module Oden.Output.Compiler where

import           Text.PrettyPrint

import           Oden.Compiler
import           Oden.Compiler.Monomorphization
import           Oden.Output
import           Oden.Output.Instantiate ()

instance OdenOutput MonomorphError where
  outputType _ = Error
  name (NotInScope _)                   = "Compiler.Monomorph.NotInScope"
  name (AmbigiousReference _ _)         = "Compiler.Monomorph.AmbigiousReference"
  name (UnexpectedPolyType _ _)         = "Compiler.Monomorph.UnexpectedPolyType"
  name (MonomorphInstantiateError err)  = name err

  header (NotInScope i) s                   = code s i <+> text "is not in scope"
  header (AmbigiousReference i _) s         = text "Ambigious reference" <+> code s i
  header (UnexpectedPolyType _ e) s         = text "Unexpected polymorphic type" <+> code s e
  header (MonomorphInstantiateError err) s  = header err s

  details (NotInScope _) _           = empty
  details (UnexpectedPolyType _ _) _ = text "This can usually be fixed by adding (stricter) type signatures to top-level forms."
  details _ _                        = empty

  sourceInfo (MonomorphInstantiateError e) = sourceInfo e
  sourceInfo (UnexpectedPolyType si _) = Just si
  sourceInfo _ = Nothing

instance OdenOutput CompilationError where
  outputType (MonomorphError e) = outputType e
  name (MonomorphError e)       = name e
  header (MonomorphError e)     = header e
  details (MonomorphError e)    = details e
  sourceInfo (MonomorphError e) = sourceInfo e
