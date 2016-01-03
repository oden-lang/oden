module Oden.Syntax where

import qualified Oden.Core.Untyped as Untyped
import           Oden.Identifier

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
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data Import = Import PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageName [Import] [Definition]
             deriving (Show, Eq, Ord)

explodeExpr :: Expr -> Untyped.Expr
explodeExpr (Symbol i) =
  Untyped.Symbol i
explodeExpr (Literal (Bool b)) =
  Untyped.Literal (Untyped.Bool b)
explodeExpr (Literal (Int i)) =
  Untyped.Literal (Untyped.Int i)
explodeExpr (Literal (String s)) =
  Untyped.Literal (Untyped.String s)
explodeExpr (If c t f) =
  Untyped.If (explodeExpr c) (explodeExpr t) (explodeExpr f)
-- (f)
explodeExpr (Application f []) =
  Untyped.NoArgApplication (explodeExpr f)
-- (f x)
explodeExpr (Application f [p]) =
  Untyped.Application (explodeExpr f) (explodeExpr p)
-- (f x y z)
explodeExpr (Application f (p:ps)) =
  explodeExpr (Application (Application f [p]) ps)
explodeExpr (Fn [] b) =
  Untyped.NoArgFn (explodeExpr b)
explodeExpr (Fn [arg] b) =
  Untyped.Fn arg (explodeExpr b)
explodeExpr (Fn (arg:args) b) =
  Untyped.Fn arg (explodeExpr (Fn args b))
-- invalid, but can be handled anyway
explodeExpr (Let [] b) = explodeExpr b
explodeExpr (Let [(n, e)] b) =
  Untyped.Let n (explodeExpr e) (explodeExpr b)
explodeExpr (Let ((n, e):bs) b) =
  Untyped.Let n (explodeExpr e) (explodeExpr (Let bs b))
explodeExpr (Slice es) =
  Untyped.Slice (map explodeExpr es)

explodeImport :: Import -> Untyped.Import
explodeImport (Import name) = Untyped.Import name

explodeDefinition :: Definition -> Untyped.Definition
explodeDefinition (ValueDefinition name expr) =
  Untyped.Definition name (explodeExpr expr)
explodeDefinition (FnDefinition name args body) =
  Untyped.Definition name (explodeExpr (Fn args body))

explodePackage :: Package -> Untyped.Package
explodePackage (Package name imports definitions) =
  Untyped.Package name is ds
  where is = map explodeImport imports
        ds = map explodeDefinition definitions
