{-# LANGUAGE TypeSynonymInstances #-}
module Oden.Core where

import Oden.Identifier
import qualified Oden.Type.Polymorphic as Poly

data Expr t = Symbol Identifier t
            | Application (Expr t) (Expr t) t
            | NoArgApplication (Expr t) t
            | Fn Name (Expr t) t
            | NoArgFn (Expr t) t
            | Let Name (Expr t) (Expr t) t
            | Literal Literal t
            | If (Expr t) (Expr t) (Expr t) t
            | Fix (Expr t) t
            deriving (Show, Eq, Ord)

typeOf :: Expr t -> t
typeOf (Symbol _ t) = t
typeOf (Application _ _ t) = t
typeOf (NoArgApplication _ t) = t
typeOf (Fn _ _ t) = t
typeOf (NoArgFn _ t) = t
typeOf (Let _ _ _ t) = t
typeOf (Literal _ t) = t
typeOf (If _ _ _ t) = t
typeOf (Fix _ t) = t

data Literal = Int Integer
             | Bool Bool
             | String String
             deriving (Show, Eq, Ord)

type CanonicalExpr = (Poly.Scheme, Expr Poly.Type)

data Definition = Definition Name CanonicalExpr
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data Import = Import PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageName [Import] [Definition]
             deriving (Show, Eq, Ord)
