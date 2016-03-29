module Oden.Output.Backend where

import           Text.PrettyPrint.Leijen

import           Oden.Backend
import           Oden.Output

instance OdenOutput CodegenError where
  outputType _ = Error
  name (UnexpectedError _)      = "Backend.UnexpectedError"
  header (UnexpectedError _) _  = text "Unexpected error in codegen"
  details (UnexpectedError s) _ = text s
  sourceInfo _ = Nothing
