{-# LANGUAGE OverloadedStrings #-}

module Oden.Parser (
  parseExpr,
  parseDefinition,
  parsePackage
) where

import           Data.List
import qualified Data.Text.Lazy        as L
import           Text.Parsec
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token     as Tok

import           Oden.Identifier
import           Oden.Lexer            as Lexer
import           Oden.Syntax           as Syntax

sign :: Parser (Integer -> Integer)
sign = (char '-' >> return negate)
       <|> (char '+' >> return id)
       <|> return id

integer :: Parser Integer
integer = do
  f <- sign
  n <- Tok.natural lexer
  return (f n)

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
  s <- Lexer.string
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

slice :: Parser Expr
slice = Slice <$> (char '!' *> brackets (expr `sepBy` whitespace))

expr :: Parser Expr
expr =
  bool
  <|> try number
  <|> parens (fn
              <|> if'
              <|> let'
              <|> application)
  <|> stringLiteral
  <|> slice
  <|> symbol

tvar :: Parser String
tvar = char '#' *> identifier

type' :: Parser TypeExpr
type' = slice'
        <|> var
        <|> any'
        <|> con
        <|> parens (noArgFn <|> fn')
  where
  var = TEVar <$> tvar
  any' = reserved "any" *> return TEAny
  con = TECon <$> identifier
  fn' = do
    d <- type'
    _ <- rArrow
    TEFn d <$> (type' `sepBy1` rArrow)
  noArgFn = TENoArgFn <$> (rArrow *> type')
  slice' = TESlice <$> (char '!' *> brackets type')


definition :: Parser Definition
definition = parens (typeSignature <|> def)
  where
  explicitlyQuantifiedType = parens (reserved "forall" *>
                                    (Explicit <$> parens (many tvar)
                                              <*> type'))
  implicitlyQuantifiedType = Implicit <$> type'
  signature = try explicitlyQuantifiedType <|> implicitlyQuantifiedType
  typeSignature = reserved ":" *> (TypeSignature <$> identifier <*> signature)
  valueDef = ValueDefinition <$> identifier <*> expr
  fnDef =
    parens (FnDefinition <$> identifier <*> many identifier) <*> expr
  def = reserved "def" *> (fnDef <|> valueDef)

pkgDecl :: Parser [Name]
pkgDecl = parens $ do
  reserved "pkg"
  packageName

import' :: Parser Import
import' = parens $ do
  reserved "import"
  Import <$> importName

pkg :: Parser Package
pkg = Package <$> pkgDecl <*> many (try import') <*> many definition

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseDefinition :: L.Text -> Either ParseError Definition
parseDefinition = parse (contents definition) "<stdin>"

parsePackage ::  FilePath -> L.Text -> Either ParseError Package
parsePackage = parse (contents pkg)
