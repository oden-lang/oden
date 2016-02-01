{-# LANGUAGE TypeSynonymInstances #-}
module Oden.Core where

import Oden.Identifier
import Oden.Core.Operator
import qualified Oden.Type.Polymorphic as Poly

import Data.List

data Expr t = Symbol Identifier t
            | Op BinaryOperator (Expr t) (Expr t) t
            | Application (Expr t) (Expr t) t
            | NoArgApplication (Expr t) t
            | UncurriedFnApplication (Expr t) [Expr t] t
            | Fn Name (Expr t) t
            | NoArgFn (Expr t) t
            | Let Name (Expr t) (Expr t) t
            | Literal Literal t
            | Slice [Expr t] t
            | If (Expr t) (Expr t) (Expr t) t
            | Block [Expr t] t
            deriving (Eq, Ord)

instance Show t => Show (Expr t) where
  show (Symbol i t) = "(" ++ show i ++ " : " ++ show t ++ ")"
  show (Op o e1 e2 t) = "(" ++ show e1 ++ " " ++ show o ++ " " ++ show e2 ++ " :: " ++ show t ++ ")"
  show (Application a b t) = "((" ++ show a  ++ " " ++ show b ++ ") : " ++ show t ++ ")"
  show (NoArgApplication a t) = "((" ++ show a ++ ") : " ++ show t ++ ")"
  show (UncurriedFnApplication f p t) = "((" ++ show f ++ " " ++ show p ++  ") : " ++ show t ++ ")"
  show (Fn n b t) = "((fn (" ++ n ++ ") " ++ show b ++ ") : " ++ show t ++ ")"
  show (NoArgFn b t) = "((fn () " ++ show b ++ ") : " ++ show t ++ ")"
  show (Let n e b t) = "((let ((" ++ n ++ " " ++ show e ++ ")) " ++ show b ++ ") : " ++ show t ++ ")"
  show (Literal l _) = show l
  show (If ce te ee _) = "(if " ++ show ce ++ " " ++ show te ++ "" ++ show ee ++ ")"
  show (Slice exprs _) = "![" ++ concatMap show exprs ++ "]"
  show (Block exprs _) = "{" ++ intercalate "; " (map show exprs) ++ "}"

typeOf :: Expr t -> t
typeOf (Symbol _ t) = t
typeOf (Op _ _ _ t) = t
typeOf (Application _ _ t) = t
typeOf (NoArgApplication _ t) = t
typeOf (UncurriedFnApplication _ _ t) = t
typeOf (Fn _ _ t) = t
typeOf (NoArgFn _ t) = t
typeOf (Let _ _ _ t) = t
typeOf (Literal _ t) = t
typeOf (If _ _ _ t) = t
typeOf (Slice _ t) = t
typeOf (Block _ t) = t

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Eq, Ord)

instance Show Literal where
  show (Int n) = show n
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (String s) = show s
  show Unit = "{}"

type CanonicalExpr = (Poly.Scheme, Expr Poly.Type)

data Definition = Definition Name CanonicalExpr
                deriving (Eq, Ord)

instance Show Definition where
  show (Definition name (sc, te)) =
    name ++ " :: " ++ show sc
    ++ "\n" ++ name ++ " = " ++ show te

type PackageName = [Name]

data Import = Import PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageName [Import] [Definition]
             deriving (Show, Eq, Ord)
