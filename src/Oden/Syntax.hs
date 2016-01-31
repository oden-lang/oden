module Oden.Syntax where

import           Oden.Identifier
import           Oden.Core.Operator

type Binding = (Name, Expr)

data Expr = Symbol Identifier
          | Op BinaryOperator Expr Expr
          | Application Expr [Expr]
          | Fn [Name] Expr
          | Let [Binding] Expr
          | Literal Literal
          | If Expr Expr Expr
          | Slice [Expr]
          deriving (Show, Eq, Ord)

data Literal = Int Integer
             | Bool Bool
             | String String
             deriving (Show, Eq, Ord)

data TypeExpr = TEAny
              | TEVar String
              | TECon String
              | TEFn TypeExpr [TypeExpr]
              | TENoArgFn TypeExpr
              | TESlice TypeExpr
              deriving (Show, Eq, Ord)

data SchemeExpr = Explicit [String] TypeExpr
                | Implicit TypeExpr
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data TopLevel = ImportDeclaration PackageName
                | TypeSignature Name SchemeExpr
                | ValueDefinition Name Expr
                | FnDefinition Name [Name] Expr
                deriving (Show, Eq, Ord)

data Package = Package PackageName [TopLevel]
             deriving (Show, Eq, Ord)
