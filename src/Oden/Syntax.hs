module Oden.Syntax where

import           Oden.Identifier
import           Oden.Core.Operator
import           Oden.SourceInfo
import           Oden.Type.Signature

data NameBinding = NameBinding SourceInfo Name
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

type PackageName = [Name]

data PackageDeclaration = PackageDeclaration SourceInfo PackageName
                          deriving (Show, Eq, Ord)

data StructFieldExpr = StructFieldExpr SourceInfo Name SignatureExpr
                     deriving (Show, Eq, Ord)

data TopLevel = ImportDeclaration SourceInfo PackageName
              | TypeSignatureDeclaration SourceInfo Name TypeSignature
              | ValueDefinition SourceInfo Name Expr
              | FnDefinition SourceInfo Name [NameBinding] Expr
              | StructDefinition SourceInfo Name [NameBinding] [StructFieldExpr]
              deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [TopLevel]
             deriving (Show, Eq, Ord)
