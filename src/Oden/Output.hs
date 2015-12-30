module Oden.Output where

import Text.PrettyPrint
import Data.List

data OutputType = Warning | Error

-- TODO: Add source info
class OdenOutput e where
  outputType :: e -> OutputType
  name :: e -> String
  header :: e -> Doc
  details :: e -> Doc

backtick :: Doc
backtick = text "`"

escape :: [Int] -> Doc
escape ns = text ("\ESC[" ++ intercalate ";" (map show ns)) <> text "m"

code :: Show a => a -> Doc
code a = escape [1, 34] <> backtick <> text (show a) <> backtick <> escape [0]

formatOutputType :: OdenOutput e => e -> Doc
formatOutputType e =
  case outputType e of
    Warning -> escape [1, 33] <> text "warning:" <> escape [0]
    Error -> escape [1, 31] <> text "error:" <> escape [0]

wikiLink :: OdenOutput e => e -> Doc
wikiLink e = text "For more information see:"
             <+> text "https://github.com/oden-lang/oden/wiki/Error:-"
             <> text (name e)

format :: OdenOutput e => e -> Doc
format e = formatOutputType e <+> header e
      $+$ nest 2 (details e)
      $+$ wikiLink e

print :: OdenOutput e => e -> String
print = render . format
