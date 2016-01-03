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
            | Slice [Expr t] t
            | If (Expr t) (Expr t) (Expr t) t
            | Fix (Expr t) t
            deriving (Eq, Ord)

instance Show t => Show (Expr t) where
  show (Symbol i t) = "(" ++ show i ++ " : " ++ show t ++ ")"
  show (Application a b t) = "((" ++ show a  ++ " " ++ show b ++ ") : " ++ show t ++ ")"
  show (NoArgApplication a t) = "((" ++ show a ++ ") : " ++ show t ++ ")"
  show (Fn n b t) = "((fn (" ++ n ++ ") " ++ show b ++ ") : " ++ show t ++ ")"
  show (NoArgFn b t) = "((fn () " ++ show b ++ ") : " ++ show t ++ ")"
  show (Let n e b t) = "((let ((" ++ n ++ " " ++ show e ++ ")) " ++ show b ++ ") : " ++ show t ++ ")"
  show (Literal l _) = show l
  show (If ce te ee t) = "(if " ++ show ce ++ " " ++ show te ++ "" ++ show ee ++ ")"
  show (Slice exprs _) = "![" ++ concatMap show exprs ++ "]"

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
typeOf (Slice _ t) = t

data Literal = Int Integer
             | Bool Bool
             | String String
             deriving (Eq, Ord)

instance Show Literal where
  show (Int n) = show n
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (String s) = show s

type CanonicalExpr = (Poly.Scheme, Expr Poly.Type)

data Definition = Definition Name CanonicalExpr
                deriving (Eq, Ord)

instance Show Definition where
  show (Definition name (sc, te)) = "(def " ++ name ++ " " ++ show sc ++ " " ++ show te ++ ")"

type PackageName = [Name]

data Import = Import PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageName [Import] [Definition]
             deriving (Show, Eq, Ord)
