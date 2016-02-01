{-# LANGUAGE OverloadedStrings #-}

module Oden.Parser where

import qualified Data.Text.Lazy        as L
import           Text.Parsec
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token     as Tok
import qualified Text.Parsec.Expr      as Ex

import           Oden.Identifier
import           Oden.Lexer            as Lexer
import           Oden.Syntax           as Syntax
import           Oden.Core.Operator

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
symbol = Symbol <$> (try qualified <|> unqualified)
  where
  qualified = Qualified <$> identifier <*> (char '.' *> identifier)
  unqualified = Unqualified <$> identifier

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

block :: Parser Expr
block = braces (whitespace *> expr <* whitespace)

if' :: Parser Expr
if' = do
  reserved "if"
  cond <- expr
  spaces
  reserved "then"
  spaces
  t <- expr
  spaces
  reserved "else"
  f <- expr
  return (If cond t f)

fn :: Parser Expr
fn = do
  reserved "fn"
  Fn <$> many identifier <*> (spaces *> rArrow *> spaces *> expr)

binding :: Parser Binding
binding = do
  i <- identifier
  reservedOp "="
  e <- expr
  return (i, e)

let' :: Parser Expr
let' = do
  reserved "let"
  bindings <- many1 binding
  reserved "in"
  Let bindings <$> expr

application :: Parser Expr
application = do
  f <- symbol <|> parens expr
  args <- parensList expr
  return $ Application f args

slice :: Parser Expr
slice = Slice <$> (char '!' *> brackets (expr `sepBy` comma))

aexp :: Parser Expr
aexp =
  fn
  <|> if'
  <|> let'
  <|> stringLiteral
  <|> slice
  <|> bool
  <|> try number
  <|> try application
  <|> symbol
  <|> block
  <|> parens expr

tvar :: Parser String
tvar = char '#' *> identifier

type' :: Parser TypeExpr
type' = do
  ts <- simple `sepBy1` rArrow
  case ts of
    [t] -> return t
    (d:r) -> return (TEFn d r)
    [] -> fail ""
  where
  simple = slice'
        <|> noArgFn
        <|> any'
        <|> con
        <|> var
        <|> parens type'
  var = TEVar <$> tvar
  any' = reserved "any" *> return TEAny
  con = TECon <$> identifier
  noArgFn = TENoArgFn <$> (rArrow *> type')
  slice' = TESlice <$> (char '!' *> brackets type')

expr :: Parser Expr
expr = Ex.buildExpressionParser table aexp

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table = [
    [
      infixOp "*" (Op Multiply) Ex.AssocLeft,
      infixOp "/" (Op Divide) Ex.AssocLeft
    ],
    [
      infixOp "+" (Op Add) Ex.AssocLeft,
      infixOp "-" (Op Subtract) Ex.AssocLeft,

      infixOp "++" (Op Concat) Ex.AssocLeft
    ],
    [
      infixOp "<" (Op LessThan) Ex.AssocLeft,
      infixOp ">" (Op GreaterThan) Ex.AssocLeft,
      infixOp "<=" (Op LessThanEqual) Ex.AssocLeft,
      infixOp ">=" (Op GreaterThanEqual) Ex.AssocLeft,
      infixOp "==" (Op Equals) Ex.AssocLeft
    ],
    [
      infixOp "&&" (Op And) Ex.AssocLeft,
      infixOp "||" (Op Or) Ex.AssocLeft
    ]
  ]

pkgDecl :: Parser [Name]
pkgDecl = reserved "package" *> packageName <* topSeparator

topLevel :: Parser TopLevel
topLevel = import' <|> try typeSignature <|> def
  where
  explicitlyQuantifiedType = do
    reservedOp "forall"
    vars <- many1 tvar
    reserved "."
    Explicit vars <$> type'
  implicitlyQuantifiedType = Implicit <$> type'
  signature = try explicitlyQuantifiedType <|> implicitlyQuantifiedType
  typeSignature = do
    i <- identifier
    reservedOp "::"
    TypeSignature i <$> signature
  valueDef = do
    i <- identifier
    reservedOp "="
    ValueDefinition i <$> expr
  fnDef =
    FnDefinition <$> identifier <*> many identifier <*> (rArrow *> expr)
  def = try valueDef <|> fnDef
  import' = do
    reserved "import"
    ImportDeclaration <$> importName

package :: Parser Package
package = Package <$> pkgDecl <*> topLevel `sepBy` topSeparator

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseTopLevel :: L.Text -> Either ParseError TopLevel
parseTopLevel = parse (contents topLevel) "<stdin>"

parsePackage ::  FilePath -> L.Text -> Either ParseError Package
parsePackage = parse (contents package)
