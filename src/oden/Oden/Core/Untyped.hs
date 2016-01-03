module Oden.Core.Untyped where

import           Oden.Identifier

data Expr = Symbol Identifier
          | Application Expr [Expr]
          | Fn Name Expr
          | NoArgFn Expr
          | Let Name Expr Expr
          | Literal Literal
          | If Expr Expr Expr
          | Slice [Expr]
          deriving (Show, Eq, Ord)

data Literal = Int Integer
             | Bool Bool
             | String String
             deriving (Show, Eq, Ord)

data Definition = Definition Name Expr
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data Import = Import PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageName [Import] [Definition]
             deriving (Show, Eq, Ord)
