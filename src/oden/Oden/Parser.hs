{-# LANGUAGE OverloadedStrings #-}

module Oden.Parser (
  parseExpr,
  parseDefinition,
  parsePackage
) where

import           Data.List
import qualified Data.Set              as Set
import qualified Data.Text.Lazy        as L
import           Text.Parsec
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token     as Tok

import           Oden.Identifier
import           Oden.Lexer            as Lexer
import           Oden.Syntax           as Syntax
import           Oden.Type.Polymorphic

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

tvar :: Parser TVar
tvar = char '#' *> (TV <$> identifier)

type' :: Parser Type
type' = slice'
        <|> var
        <|> any'
        <|> con
        <|> parens (noArgFn <|> fn')
  where
  var = TVar <$> tvar
  any' = reserved "any" *> return TAny
  con = TCon <$> identifier
  fn' = TFn <$> type' <*> (reserved "->" *> type')
  noArgFn = TNoArgFn <$> (reserved "->" *> type')
  slice' = TSlice <$> (char '!' *> brackets type')

explicitlyQuantifiedType :: Parser Scheme
explicitlyQuantifiedType = parens (reserved "forall" *>
                         (Forall <$> parens (many tvar)
                                 <*> type'))
implicitlyQuantifiedType :: Parser Scheme
implicitlyQuantifiedType = do
  t <- type'
  return (Forall (Set.toList (ftv t)) t)

definition :: Parser Definition
definition = parens (typeSignature <|> def)
  where
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
