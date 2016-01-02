{-# LANGUAGE FlexibleContexts #-}
module Oden.Output where

import           Control.Monad.Reader
import           Data.List
import           Text.PrettyPrint

data OutputSettings = OutputSettings { markdown   :: Bool
                                     , monochrome :: Bool }
data OutputType = Warning | Error

data Output a = Reader OutputSettings a

-- TODO: Add source info
class OdenOutput e where
  outputType :: e -> OutputType
  name :: e -> String
  header :: e -> OutputSettings -> Doc
  details :: e -> OutputSettings -> Doc

backtick :: Doc
backtick = text "`"

escape :: OutputSettings -> [Int] -> Doc
escape OutputSettings{monochrome = True} _ = empty
escape OutputSettings{monochrome = False} ns = text ("\ESC[" ++ intercalate ";" (map show ns)) <> text "m"

strCode :: OutputSettings -> String -> Doc
strCode settings a =
  escape settings [1, 34] <> contents <> escape settings [0]
  where contents = backtick <> text a <> backtick

code :: Show a => OutputSettings -> a -> Doc
code s = strCode s . show

formatOutputType :: (MonadReader OutputSettings m, OdenOutput e) => e -> m Doc
formatOutputType e = do
  s <- ask
  case outputType e of
    Warning -> return $ escape s [1, 33] <> text "Warning:" <> escape s [0]
    Error -> return $ escape s [1, 31] <> text "Error:" <> escape s [0]

wikiLink :: (MonadReader OutputSettings m, OdenOutput e) => e -> m Doc
wikiLink e = do
 s <- ask
 return $ text "For more information see:"
          <+> text "https://github.com/oden-lang/oden/wiki/Error:-" <> text (name e)

format :: (MonadReader OutputSettings m, OdenOutput e) => e -> m Doc
format e = do
  s <- ask
  t <- formatOutputType e
  wl <- wikiLink e
  return (t <+> header e s $+$ nest 2 (details e s) $+$ wl)

print :: OdenOutput e => OutputSettings -> e -> String
print settings e = render $ runReader (format e) settings
