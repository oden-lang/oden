{-# LANGUAGE OverloadedStrings #-}

module Oden.Parser where

import           Control.Monad (void)
import qualified Data.Text.Lazy        as L
import           Text.Parsec
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token     as Tok
import qualified Text.Parsec.Expr      as Ex

import           Oden.Identifier
import           Oden.Lexer            hiding (identifier)
import qualified Oden.Lexer            as Lexer
import           Oden.Syntax           as Syntax
import           Oden.Type.Signature
import           Oden.SourceInfo
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

currentSourceInfo :: Parser SourceInfo
currentSourceInfo = do
  pos <- getPosition
  return (SourceInfo (Position (sourceName pos)
                               (sourceLine pos)
                               (sourceColumn pos)))

identifier :: Parser Identifier
identifier = do
  s <- Lexer.identifier
  either (fail . errorMessage) return (createLegalIdentifier s)

identified :: (SourceInfo -> Identifier -> e) -> Parser e
identified f = f <$> currentSourceInfo <*> identifier

symbol :: Parser Expr
symbol = identified Symbol

number :: Parser Expr
number = do
  si <- currentSourceInfo
  n <- integer
  return (Literal si (Int (fromIntegral n)))

stringLiteral :: Parser Expr
stringLiteral = do
  si <- currentSourceInfo
  s <- Lexer.string
  return (Literal si (Syntax.String s))

bool :: Parser Expr
bool = do
  si <- currentSourceInfo
  (reserved "true" >> return (Literal si (Bool True)))
   <|> (reserved "false" >> return (Literal si (Bool False)))

block :: Parser Expr
block = do
  si <- currentSourceInfo
  exprs <- braces (whitespace *> (expr `sepBy1` topSeparator) <* whitespace)
  return (Block si exprs)

if' :: Parser Expr
if' = do
  si <- currentSourceInfo
  reserved "if"
  cond <- expr
  spaces
  reserved "then"
  spaces
  t <- expr
  spaces
  reserved "else"
  f <- expr
  return (If si cond t f)

nameBinding :: Parser NameBinding
nameBinding = NameBinding <$> currentSourceInfo <*> identifier

fn :: Parser Expr
fn = do
  si <- currentSourceInfo
  args <- parensList nameBinding
  rArrow
  body <- expr
  return (Fn si args body)

letPair :: Parser LetPair
letPair = do
  si <- currentSourceInfo
  i <- nameBinding
  equals
  e <- expr
  return (LetPair si i e)

let' :: Parser Expr
let' = do
  si <- currentSourceInfo
  reserved "let"
  bindings <- many1 letPair
  reserved "in"
  body <- expr
  return (Let si bindings body)

structInitializer :: Parser Expr
structInitializer = do
  si <- currentSourceInfo
  t <- type'
  args <- bracesList expr
  return (StructInitializer si t args)

emptyBrackets :: Parser ()
emptyBrackets = do
  _ <- char '['
  _ <- char ']'
  return ()

slice :: Parser Expr
slice = do
  si <- currentSourceInfo
  emptyBrackets
  exprs <- braces (expr `sepBy` comma)
  return (Slice si exprs)

unitExprOrTuple :: Parser Expr
unitExprOrTuple = do
  si <- currentSourceInfo
  exprs <- parensList expr
  case exprs of
    []      -> return (Literal si Unit)
    [e]     -> return e
    (f:s:r) -> return (Tuple si f s r)

termNoSlice :: Parser Expr
termNoSlice =
  try fn
  <|> if'
  <|> let'
  <|> stringLiteral
  <|> slice
  <|> bool
  <|> try number
  <|> try structInitializer
  <|> symbol
  <|> block
  <|> unitExprOrTuple

subscript :: Parser Subscript
subscript = try openStart <|> try range <|> try openEnd <|> simple
   where
     openStart = RangeTo <$> (char ':' *> expr)
     openEnd = RangeFrom <$> (expr <* char ':')
     range = Range <$> expr <*> (char ':' *> expr)
     simple = Singular <$> expr

subscriptExpr :: Parser Expr
subscriptExpr = do
  si <- currentSourceInfo
  basicTerm <- termNoSlice
  indices <- many (brackets subscript)
  case indices of
    [] -> return basicTerm
    is -> return (Subscript si basicTerm is)

tvar :: Parser String
-- TODO: Parse type variables as just like identifiers.
tvar = char '#' *> (asString <$> identifier)

type' :: Parser SignatureExpr
type' = do
  si <- currentSourceInfo
  ts <- simple `sepBy1` rArrow
  return (foldr1 (TSFn si) ts)
  where
  simple :: Parser SignatureExpr
  simple = slice'
        <|> noArgFn
        <|> identified TSSymbol
        <|> unitExprOrTupleType
        <|> structType
  noArgFn = TSNoArgFn <$> currentSourceInfo <*> (rArrow *> type')
  unitExprOrTupleType = do
    si <- currentSourceInfo
    ts <- parensList type'
    case ts of
      [] -> return (TSUnit si)
      [t] -> return t
      (f:s:r) -> return (TSTuple si f s r)
  slice' = TSSlice <$> currentSourceInfo
                   <*> (emptyBrackets *> braces type')
  structType = TSStruct <$> currentSourceInfo
                      <*> braces (structFieldType `sepBy1` structFieldSeparator)
  structFieldType = do
    si <- currentSourceInfo
    n <- identifier
    TSStructField si n <$> type'

  structFieldSeparator = do
    spaces
    optional (comma <|> void newline)
    whitespace


expr :: Parser Expr
expr = Ex.buildExpressionParser table subscriptExpr

memberAccess :: Ex.Assoc -> Op Expr
memberAccess = Ex.Infix $ do
  reservedOp "."
  return (\expr' name -> MemberAccess (getSourceInfo expr') expr' name)

application :: Op Expr
application = Ex.Postfix $ do
  si <- currentSourceInfo
  args <- parensList expr
  return (\f -> Application si f args)

infixOp :: String -> BinaryOperator -> Ex.Assoc -> Op Expr
infixOp x o = Ex.Infix $ do
  si <- currentSourceInfo
  reservedOp x
  return (BinaryOp si o)

prefixOp :: String -> UnaryOperator -> Op Expr
prefixOp x o = Ex.Prefix $ do
  si <- currentSourceInfo
  reservedOp x
  return (UnaryOp si o)

table :: Operators Expr
table = [
    [
      memberAccess Ex.AssocLeft
    ],
    [
      application
    ],
    [
      prefixOp "+" Positive,
      prefixOp "-" Negative,
      prefixOp "!" Not
    ],
    [
      infixOp "*" Multiply Ex.AssocLeft,
      infixOp "/" Divide Ex.AssocLeft
    ],
    [
      infixOp "+" Add Ex.AssocLeft,
      infixOp "-" Subtract Ex.AssocLeft,

      infixOp "++" Concat Ex.AssocLeft
    ],
    [
      infixOp "<" LessThan Ex.AssocLeft,
      infixOp ">" GreaterThan Ex.AssocLeft,
      infixOp "<=" LessThanEqual Ex.AssocLeft,
      infixOp ">=" GreaterThanEqual Ex.AssocLeft,
      infixOp "==" Equals Ex.AssocLeft
    ],
    [
      infixOp "&&" And Ex.AssocLeft,
      infixOp "||" Or Ex.AssocLeft
    ]
  ]

pkgDecl :: Parser PackageDeclaration
pkgDecl = do
  si <- currentSourceInfo
  reserved "package"
  pkg <- packageName
  topSeparator
  return (PackageDeclaration si pkg)

topLevel :: Parser TopLevel
topLevel = import' <|> typeDef <|> try typeSignature <|> def
  where
  tvarBinding = identified SignatureVarBinding
  explicitlyQuantifiedType = do
    si <- currentSourceInfo
    reservedOp "forall"
    bindings <- many1 tvarBinding
    reservedOp "."
    TypeSignature si bindings <$> type'
  implicitlyQuantifiedType = do
    si <- currentSourceInfo
    TypeSignature si [] <$> type'
  signature = try explicitlyQuantifiedType <|> implicitlyQuantifiedType
  typeSignature = do
    si <- currentSourceInfo
    i <- identifier
    reservedOp "::"
    TypeSignatureDeclaration si i <$> signature
  valueDef si n = do
    equals
    ValueDefinition si n <$> expr
  fnDef si n =
    FnDefinition si n <$> parensList nameBinding <*> (equals *> expr)
  def = do
    si <- currentSourceInfo
    n <- identifier
    fnDef si n <|> valueDef si n
  import' = do
    si <- currentSourceInfo
    reserved "import"
    ImportDeclaration si <$> importName
  typeDef = do
    si <- currentSourceInfo
    reserved "type"
    n <- identifier
    equals
    TypeDefinition si n <$> type'

package :: Parser Package
package = Package <$> pkgDecl <*> topLevel `sepBy` topSeparator

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseTopLevel :: L.Text -> Either ParseError TopLevel
parseTopLevel = parse (contents topLevel) "<stdin>"

parsePackage ::  FilePath -> L.Text -> Either ParseError Package
parsePackage = parse (contents package)
