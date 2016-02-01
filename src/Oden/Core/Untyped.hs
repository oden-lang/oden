module Oden.Core.Untyped where

import           Oden.Identifier
import           Oden.Core.Operator
import           Oden.Type.Polymorphic

data Expr = Symbol Identifier
          | Op BinaryOperator Expr Expr
          | Application Expr [Expr]
          | Fn Name Expr
          | NoArgFn Expr
          | Let Name Expr Expr
          | Literal Literal
          | If Expr Expr Expr
          | Slice [Expr]
          | Block [Expr]
          deriving (Show, Eq, Ord)

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

data Definition = Definition Name (Maybe Scheme) Expr
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data Import = Import PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageName [Import] [Definition]
             deriving (Show, Eq, Ord)
