module Oden.Syntax where

import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.SourceInfo
import           Oden.Type.Signature

data NameBinding = NameBinding SourceInfo Identifier
                 deriving (Show, Eq, Ord)

data LetPair = LetPair SourceInfo NameBinding Expr
             deriving (Show, Eq, Ord)

data Expr = Symbol SourceInfo Identifier
          | Subscript SourceInfo Expr [Subscript]
          | UnaryOp SourceInfo UnaryOperator Expr
          | BinaryOp SourceInfo BinaryOperator Expr Expr
          | Application SourceInfo Expr [Expr]
          | Fn SourceInfo [NameBinding] Expr
          | Let SourceInfo [LetPair] Expr
          | Literal SourceInfo Literal
          | If SourceInfo Expr Expr Expr
          | Slice SourceInfo [Expr]
          | Tuple SourceInfo Expr Expr [Expr]
          | Block SourceInfo [Expr]
          | StructInitializer SourceInfo SignatureExpr [Expr]
          deriving (Show, Eq, Ord)

data Subscript = Singular Expr
               | RangeTo Expr
               | RangeFrom Expr
               | Range Expr Expr
               deriving (Show, Eq, Ord)

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

type PackageName = [String]

data PackageDeclaration = PackageDeclaration SourceInfo PackageName
                          deriving (Show, Eq, Ord)

data StructFieldExpr = StructFieldExpr SourceInfo Identifier SignatureExpr
                     deriving (Show, Eq, Ord)

data TopLevel = ImportDeclaration SourceInfo PackageName
              | TypeSignatureDeclaration SourceInfo Identifier TypeSignature
              | ValueDefinition SourceInfo Identifier Expr
              | FnDefinition SourceInfo Identifier [NameBinding] Expr
              -- TODO: Add support for type parameters
              | TypeDefinition SourceInfo Identifier SignatureExpr
              deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [TopLevel]
             deriving (Show, Eq, Ord)
