module Oden.Output.Parser where

import Text.PrettyPrint
import Text.Parsec
import Text.Parsec.Error

import Oden.Output as Output
import Oden.Parser

formatErrorMessage :: Message -> Doc
formatErrorMessage (SysUnExpect s) = text "Unexpected" <+> doubleQuotes (text s)
formatErrorMessage (UnExpect s) = text "Unexpected" <+> doubleQuotes (text s)
formatErrorMessage (Expect s) = text "Expected" <+> doubleQuotes (text s)
formatErrorMessage (Message s) = text s

instance OdenOutput ParseError where
  outputType _ = Output.Error
  name _ = "Parser.ParseError"
  header e = text "Parsing failed"
  details e = vcat (map formatErrorMessage (errorMessages e))

