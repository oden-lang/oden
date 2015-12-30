{-# LANGUAGE OverloadedStrings #-}

module Oden.Parser (
  parseExpr,
  parseDefinition,
  parsePackage
) where

import           Data.List
import qualified Data.Text.Lazy        as L
import           Text.Parsec           hiding (string)
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token     as Tok
import qualified Text.PrettyPrint as Pretty

import           Oden.Identifier
import           Oden.Lexer
import           Oden.Output           as Output
import           Oden.Syntax           as Syntax

integer :: Parser Integer
integer = Tok.integer lexer

symbol :: Parser Expr
symbol = do
  x <- identifier `sepBy1` char '.'
  case x of
    [n] -> return (Symbol (Unqualified n))
    [p, n] -> return (Symbol (Qualified p n))
    names -> fail ("Invalid symbol: " ++ intercalate "." names)

number :: Parser Expr
number = do
  n <- integer
  return (Literal (Int (fromIntegral n)))

stringLiteral :: Parser Expr
stringLiteral = do
  s <- string
  return (Literal (Syntax.String s))

bool :: Parser Expr
bool = (reserved "true" >> return (Literal (Bool True)))
    <|> (reserved "false" >> return (Literal (Bool False)))

if' :: Parser Expr
if' = do
  reserved "if"
  cond <- expr
  t <- expr
  f <- expr
  return (If cond t f)

fix :: Parser Expr
fix = do
  reserved "fix"
  x <- expr
  return (Fix x)

fn :: Parser Expr
fn = do
  reserved "fn"
  Fn <$> parens (many identifier) <*> expr

binding :: Parser Binding
binding = parens $ (,) <$> identifier <*> expr

let' :: Parser Expr
let' = do
  reserved "let"
  Let <$> parens (many1 binding) <*> expr

application :: Parser Expr
application = do
  f <- expr
  args <- many expr
  return $ Application f args

expr :: Parser Expr
expr =
  bool
  <|> try number
  <|> parens (fn
              <|> if'
              <|> fix
              -- <|> try letrec
              <|> let'
              <|> application)
  <|> stringLiteral
  <|> symbol

definition :: Parser Definition
definition = parens (reserved "def" *> (fnDef <|> valueDef))
  where
  valueDef = ValueDefinition <$> identifier <*> expr
  fnDef =
    parens (FnDefinition <$> identifier <*> many identifier) <*> expr

pkgDecl :: Parser [Name]
pkgDecl = parens $ do
  reserved "pkg"
  packageName

import' :: Parser Import
import' = parens $ do
  reserved "import"
  Import <$> packageName

pkg :: Parser Package
pkg = Package <$> pkgDecl <*> many (try import') <*> many definition

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseDefinition :: L.Text -> Either ParseError Definition
parseDefinition = parse (contents definition) "<stdin>"

parsePackage ::  FilePath -> L.Text -> Either ParseError Package
parsePackage = parse (contents pkg)
