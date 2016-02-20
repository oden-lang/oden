module Oden.Output.Compiler where

import           Text.PrettyPrint

import           Oden.Compiler
import           Oden.Compiler.Monomorphization
import           Oden.Output
import           Oden.Output.Instantiate ()
import           Oden.Pretty

instance OdenOutput MonomorphError where
  outputType _ = Error
  name (NotInScope _)                   = "Compiler.Monomorph.NotInScope"
  name (UnexpectedPolyType _ _)         = "Compiler.Monomorph.UnexpectedPolyType"
  name (MonomorphInstantiateError err)  = name err

  header (NotInScope i) s                   = code s (pp i) <+> text "is not in scope"
  header (UnexpectedPolyType _ e) s         = text "Unexpected polymorphic type" <+> code s (pp e)
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
