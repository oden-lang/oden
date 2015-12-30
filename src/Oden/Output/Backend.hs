module Oden.Output.Backend where

import           Text.PrettyPrint

import           Oden.Backend
import           Oden.Output

instance OdenOutput CodegenError where
  outputType _ = Error
  name (UnexpectedError s) = "Backend.UnexpectedError"
  header (UnexpectedError s) = text "Unexpected error in codegen"
  details (UnexpectedError s) = text s

