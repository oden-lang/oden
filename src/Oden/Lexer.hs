module Oden.Lexer where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

type Op a = Ex.Operator L.Text () Identity a
type Operators a = Ex.OperatorTable L.Text () Identity a

reservedNames :: [String]
reservedNames = [
    "package",
    "import",
    "def",
    "let",
    "in",
    "fix",
    "if",
    "then",
    "else",
    "any",
    "->"
  ]

reservedOps :: [String]
reservedOps = [
  "+",
  "++",
  "-",
  "*",
  "/",
  "::"
  ]

nameLetter :: Parser Char
nameLetter = alphaNum <|> oneOf "_-'!$&*+<=>?^|~"

importLetter :: Parser Char
importLetter = nameLetter <|> oneOf "."

lexer :: Tok.GenTokenParser L.Text () Identity
lexer = Tok.makeTokenParser Tok.LanguageDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = nameLetter
  , Tok.identLetter     = nameLetter
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

name :: Parser String
name = Tok.identifier lexer

string :: Parser String
string = Tok.stringLiteral lexer

comma :: Parser ()
comma = void (Tok.comma lexer)

semi :: Parser ()
semi = void (Tok.semi lexer)

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

parensList :: Parser a -> Parser [a]
parensList p = parens (Tok.commaSep lexer p)

maybeList :: Parser [a] -> Parser [a]
maybeList p = try p <|> return []

maybeParensList :: Parser a -> Parser [a]
maybeParensList = maybeList . parensList

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

rArrow :: Parser ()
rArrow = reservedOp "->"

packageName :: Parser [String]
packageName = part `sepBy` char '/'
  where
  part = many1 nameLetter

importName :: Parser [String]
importName = part `sepBy` char '/'
  where
  part = do
    c <- nameLetter
    cs <- many1 importLetter
    return (c:cs)

topSeparator :: Parser ()
topSeparator = do
  spaces
  optional (semi <|> void newline)
  whitespace

contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r
