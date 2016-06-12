{-# LANGUAGE QuasiQuotes       #-}
module Main where

import qualified Data.Map as Map

import System.FilePath
import System.Environment
import System.Process

import Text.Pandoc.JSON
import Text.Regex.PCRE.Heavy

type IsEscaped = Bool

encloseInListingEscape :: IsEscaped -> String -> String
encloseInListingEscape True s = s
encloseInListingEscape False s = "@" ++ s ++ "@"

escapeForLatex :: IsEscaped -> String -> String
escapeForLatex isEscaped = concatMap escape
  where
  escape '$' = if isEscaped then "\\$" else "$"
  escape c = [c]

replaceDashWithLatex :: IsEscaped -> String -> String
replaceDashWithLatex isEscaped = gsub ([re|(\--)|]) toLatex
  where
  toLatex ("--":_) = encloseInListingEscape isEscaped "-{}-"
  toLatex [] = ""

replaceTagWithLatex :: IsEscaped -> String -> String
replaceTagWithLatex isEscaped = gsub ([re|<(\w+?)>(.*?)</(\w+?)>|]) toLatex
  where
  toLatex (start:contents:end:_) | start == end =
    let replacedContents = replaceWithLatex True contents
        command = case start of
                    "strong" -> "\\texttt{\\textbf{" ++ replacedContents ++ "}}"
                    "em" -> "\\texttt{\\textit{" ++ replacedContents ++ "}}"
                    "sub" -> "\\textsubscript{" ++ replacedContents ++ "}"
                    tag -> error "Unsupported HTML tag: " ++ tag
    in encloseInListingEscape isEscaped command
  toLatex (m:_) = m
  toLatex [] = ""

replaceWithLatex isEscaped =
  replaceDashWithLatex isEscaped . replaceTagWithLatex isEscaped . escapeForLatex isEscaped

postProcess :: Format -> String -> String
postProcess fmt contents
  | fmt == Format "latex" = unlines $ map (replaceWithLatex False) $ lines contents
  | otherwise = contents

includeCode :: Maybe Format -> Block -> IO Block
includeCode (Just fmt) cb@(CodeBlock (id, classes, attrs) contents) = do
  let attrs' = Map.fromList attrs
  case Map.lookup "include" attrs' of
    Just f -> do
      fileContents <- if "formatted" `Map.member` attrs'
                        then postProcess fmt <$> readFile f
                        else readFile f
      let filteredAttrs = Map.delete "include" $ Map.delete "formatted" attrs'
          classes' = unwords classes
      case fmt of
        (Format "html5") -> return (RawBlock (Format "html") ("<pre class=" ++ classes' ++ "><code>" ++ fileContents ++ "</code></pre>"))
        _               -> return (CodeBlock (id, classes, Map.toList filteredAttrs) fileContents)
    Nothing -> return cb
includeCode _ x = return x

main :: IO ()
main = toJSONFilter includeCode
