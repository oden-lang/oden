module Oden.Syntax where

import           Oden.Identifier
import           Oden.Type.Polymorphic

type Binding = (Name, Expr)

data Expr = Symbol Identifier
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

data Definition = ValueDefinition Name Expr
                | FnDefinition Name [Name] Expr
                | TypeSignature Name Scheme
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data Import = Import PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageName [Import] [Definition]
             deriving (Show, Eq, Ord)
