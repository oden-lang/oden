{-# LANGUAGE FlexibleContexts #-}
module Oden.Output where

import           Oden.SourceInfo

import           Control.Monad.Reader
import           Data.List
import           Text.PrettyPrint.Leijen hiding (line, column)

data OutputSettings = OutputSettings { markdown   :: Bool
                                     , monochrome :: Bool }
data OutputType = Warning | Error deriving (Show, Eq)

data Output a = Reader OutputSettings a

class OdenOutput e where
  outputType :: e -> OutputType
  name :: e -> String
  header :: e -> OutputSettings -> Doc
  details :: e -> OutputSettings -> Doc
  sourceInfo :: e -> Maybe SourceInfo

backtick :: Doc
backtick = text "`"

escape :: OutputSettings -> [Int] -> Doc
escape OutputSettings{monochrome = True} _ = empty
escape OutputSettings{monochrome = False} ns = text ("\ESC[" ++ intercalate ";" (map show ns)) <> text "m"

strCode :: OutputSettings -> String -> Doc
strCode settings a = code settings (text a)

code :: OutputSettings -> Doc -> Doc
code settings d =
  escape settings [1, 34] <> contents <> escape settings [0]
  where contents = backtick <> d <> backtick

formatSourceInfo :: (MonadReader OutputSettings m, OdenOutput e) => e -> m Doc
formatSourceInfo e =
  case sourceInfo e of
    Just (SourceInfo pos) ->
      return $ text (fileName pos)
               <> colon <> int (line pos)
               <> colon <> int (column pos)
               <> colon
    Just Predefined                         -> return empty
    Just Missing                            -> return empty
    Nothing                                 -> return empty

formatOutputType :: (MonadReader OutputSettings m, OdenOutput e) => e -> m Doc
formatOutputType e = do
  s <- ask
  case outputType e of
    Warning -> return $ escape s [1, 33] <> text "warning:" <> escape s [0]
    Error -> return $ escape s [1, 31] <> text "error:" <> escape s [0]

wikiLink :: (MonadReader OutputSettings m, OdenOutput e) => e -> m Doc
wikiLink e =
 return $ text "For more information see:"
          <+> text "https://github.com/oden-lang/oden/wiki/"
          <> text (show (outputType e))
          <> text ":-" <> text (name e)

format :: (MonadReader OutputSettings m, OdenOutput e) => e -> m Doc
format e = do
  s <- ask
  pos <- formatSourceInfo e
  t <- formatOutputType e
  wl <- wikiLink e
  return (vcat [
      pos <+> t <+> header e s,
      indent 2 (details e s),
      wl
    ])

print :: OdenOutput e => OutputSettings -> e -> String
print settings e = displayS (renderPretty 0.4 120 $ runReader (format e) settings) ""
