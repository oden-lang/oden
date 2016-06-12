{-# LANGUAGE QuasiQuotes       #-}
module Main where

import qualified Data.Map as Map

import System.FilePath
import System.Environment
import System.Process

import Text.Pandoc.JSON
import Text.Regex.PCRE.Heavy

import Debug.Trace

type OutputFormat = String
type IsEscaped = Bool

encloseInListingEscape :: IsEscaped -> String -> String
encloseInListingEscape True s = s
encloseInListingEscape False s = "@" ++ s ++ "@"

replaceDashWithLatex :: IsEscaped -> String -> String
replaceDashWithLatex isEscaped = gsub ([re|(\--)|]) toLatex
  where
  toLatex ("--":_) = encloseInListingEscape isEscaped "-{}-"
  toLatex [] = ""

replaceTagWithLatex :: IsEscaped -> String -> String
replaceTagWithLatex isEscaped = gsub ([re|<(\w+?)>(.*)</(\w+?)>|]) toLatex
  where
  toLatex (start:contents:end:_) | start == end =
    let replacedContents = replaceWithLatex True contents
        command = case start of
                    "strong" -> "\\texttt{\\textbf{" ++ replacedContents ++ "}}"
                    "em" -> "\\texttt{\\textit{" ++ replacedContents ++ "}}"
    in encloseInListingEscape isEscaped command
  toLatex (m:_) = m
  toLatex [] = ""

replaceWithLatex isEscaped = replaceDashWithLatex isEscaped . replaceTagWithLatex isEscaped

postProcess :: OutputFormat -> String -> String
postProcess fmt contents
  | fmt == "latex" = unlines $ map (replaceWithLatex False) $ lines contents
  | otherwise = contents

includeCode :: OutputFormat -> Block -> IO Block
includeCode fmt cb@(CodeBlock (id, classes, attrs) contents) = do
  let attrs' = Map.fromList attrs
  case Map.lookup "include" attrs' of
    Just f -> do
      fileContents <- if "formatted" `Map.member` attrs'
                        then postProcess fmt <$> readFile f
                        else readFile f
      let filteredAttrs = Map.delete "include" $ Map.delete "formatted" attrs'
      case fmt of
        "html" -> return (RawBlock (Format "html") ("<pre><code>" ++ fileContents ++ "</code></pre>"))
        _ -> return (CodeBlock (id, classes, Map.toList filteredAttrs) fileContents)
    Nothing -> return cb
includeCode _ x = return x

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fmt] -> toJSONFilter (includeCode fmt)
    _ -> error "You must specify an code output format!"
