module Oden.Lexer where

import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

reservedNames :: [String]
reservedNames = [
    "import",
    "def",
    "let",
    "fix",
    "if"
  ]

reservedOps :: [String]
reservedOps = []

identifierLetter :: Parser Char
identifierLetter = alphaNum <|> oneOf "_-'!$&*+<=>?^|~"

importLetter :: Parser Char
importLetter = identifierLetter <|> oneOf "."

lexer :: Tok.GenTokenParser L.Text () Identity
lexer = Tok.makeTokenParser Tok.LanguageDef
  { Tok.commentStart    = "#;"
  , Tok.commentEnd      = "#;"
  , Tok.commentLine     = ";"
  , Tok.nestedComments  = True
  , Tok.identStart      = identifierLetter
  , Tok.identLetter     = identifierLetter
  , Tok.opStart         = oneOf ""
  , Tok.opLetter        = oneOf ""
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

string :: Parser String
string = Tok.stringLiteral lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

packageName :: Parser [String]
packageName = part `sepBy` char '/'
  where
  part = many1 identifierLetter

importName :: Parser [String]
importName = part `sepBy` char '/'
  where
  part = do
    c <- identifierLetter
    cs <- many1 importLetter
    return (c:cs)

contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r
