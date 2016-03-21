{-# LANGUAGE TypeSynonymInstances #-}
module Oden.Core where

import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName (QualifiedName(..))
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic as Poly

data NameBinding = NameBinding (Metadata SourceInfo) Identifier
                 deriving (Show, Eq, Ord)

data FieldInitializer t = FieldInitializer (Metadata SourceInfo) Identifier (Expr t)
                        deriving (Show, Eq, Ord)

data Expr t = Symbol (Metadata SourceInfo) Identifier t
            | Subscript (Metadata SourceInfo) (Expr t) (Expr t) t
            | Subslice (Metadata SourceInfo) (Expr t) (Range t) t
            | UnaryOp (Metadata SourceInfo) UnaryOperator (Expr t) t
            | BinaryOp (Metadata SourceInfo) BinaryOperator (Expr t) (Expr t) t
            | Application (Metadata SourceInfo) (Expr t) (Expr t) t
            | NoArgApplication (Metadata SourceInfo) (Expr t) t
            | UncurriedFnApplication (Metadata SourceInfo) (Expr t) [Expr t] t
            | Fn (Metadata SourceInfo) NameBinding (Expr t) t
            | NoArgFn (Metadata SourceInfo) (Expr t) t
            | Let (Metadata SourceInfo) NameBinding (Expr t) (Expr t) t
            | Literal (Metadata SourceInfo) Literal t
            | Tuple (Metadata SourceInfo) (Expr t) (Expr t) [Expr t] t
            | If (Metadata SourceInfo) (Expr t) (Expr t) (Expr t) t
            | Slice (Metadata SourceInfo) [Expr t] t
            | Block (Metadata SourceInfo) [Expr t] t
            | RecordInitializer (Metadata SourceInfo) t [FieldInitializer t]
            | RecordFieldAccess (Metadata SourceInfo) (Expr t) Identifier t
            | PackageMemberAccess (Metadata SourceInfo) Identifier Identifier t
            deriving (Show, Eq, Ord)

instance HasSourceInfo (Expr t) where
  getSourceInfo (Symbol (Metadata si) _ _)                   = si
  getSourceInfo (Subscript (Metadata si) _ _ _)              = si
  getSourceInfo (Subslice (Metadata si) _ _ _)               = si
  getSourceInfo (UnaryOp (Metadata si) _ _ _)                = si
  getSourceInfo (BinaryOp (Metadata si) _ _ _ _)             = si
  getSourceInfo (Application (Metadata si) _ _ _)            = si
  getSourceInfo (NoArgApplication (Metadata si) _ _)         = si
  getSourceInfo (UncurriedFnApplication (Metadata si) _ _ _) = si
  getSourceInfo (Fn (Metadata si) _ _ _)                     = si
  getSourceInfo (NoArgFn (Metadata si) _ _)                  = si
  getSourceInfo (Let (Metadata si) _ _ _ _)                  = si
  getSourceInfo (Literal (Metadata si) _ _)                  = si
  getSourceInfo (If (Metadata si) _ _ _ _)                   = si
  getSourceInfo (Slice (Metadata si) _ _)                    = si
  getSourceInfo (Tuple (Metadata si) _ _ _ _)                = si
  getSourceInfo (Block (Metadata si) _ _)                    = si
  getSourceInfo (RecordInitializer (Metadata si) _ _)        = si
  getSourceInfo (RecordFieldAccess (Metadata si) _ _ _)      = si
  getSourceInfo (PackageMemberAccess (Metadata si) _ _ _)    = si

  setSourceInfo si (Symbol _ i t)                      = Symbol (Metadata si) i t
  setSourceInfo si (Subscript _ s i t)                 = Subscript (Metadata si) s i t
  setSourceInfo si (Subslice _ s r t)                  = Subslice (Metadata si) s r t
  setSourceInfo si (UnaryOp _ o r t)                   = UnaryOp (Metadata si) o r t
  setSourceInfo si (BinaryOp _ p l r t)                = BinaryOp (Metadata si) p l r t
  setSourceInfo si (Application _ f a t)               = Application (Metadata si) f a t
  setSourceInfo si (NoArgApplication _ f t)            = NoArgApplication (Metadata si) f t
  setSourceInfo si (UncurriedFnApplication _ f a t)    = UncurriedFnApplication (Metadata si) f a t
  setSourceInfo si (Fn _ n b t)                        = Fn (Metadata si) n b t
  setSourceInfo si (NoArgFn _ b t)                     = NoArgFn (Metadata si) b t
  setSourceInfo si (Let _ n v b t)                     = Let (Metadata si) n v b t
  setSourceInfo si (Literal _ l t)                     = Literal (Metadata si) l t
  setSourceInfo si (If _ c t e t')                     = If (Metadata si) c t e t'
  setSourceInfo si (Slice _ e t)                       = Slice (Metadata si) e t
  setSourceInfo si (Tuple _ f s r t)                   = Tuple (Metadata si) f s r t
  setSourceInfo si (Block _ e t)                       = Block (Metadata si) e t
  setSourceInfo si (RecordInitializer _ t vs)          = RecordInitializer (Metadata si) t vs
  setSourceInfo si (RecordFieldAccess _ expr name t)   = RecordFieldAccess (Metadata si) expr name t
  setSourceInfo si (PackageMemberAccess _ pkgAlias name t) = PackageMemberAccess (Metadata si) pkgAlias name t

typeOf :: Expr t -> t
typeOf (Symbol _ _ t) = t
typeOf (Subscript _ _ _ t) = t
typeOf (Subslice _ _ _ t) = t
typeOf (UnaryOp _ _ _ t) = t
typeOf (BinaryOp _ _ _ _ t) = t
typeOf (Application _ _ _ t) = t
typeOf (NoArgApplication _ _ t) = t
typeOf (UncurriedFnApplication _ _ _ t) = t
typeOf (Fn _ _ _ t) = t
typeOf (NoArgFn _ _ t) = t
typeOf (Let _ _ _ _ t) = t
typeOf (Literal _ _ t) = t
typeOf (If _ _ _ _ t) = t
typeOf (Tuple _ _ _ _ t) = t
typeOf (Slice _ _ t) = t
typeOf (Block _ _ t) = t
typeOf (RecordInitializer _ t _) = t
typeOf (RecordFieldAccess _ _ _ t) = t
typeOf (PackageMemberAccess _ _ _ t) = t

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

data Range t = Range (Expr t) (Expr t)
             | RangeTo (Expr t)
             | RangeFrom (Expr t)
             deriving (Show, Eq, Ord)

type CanonicalExpr = (Poly.Scheme, Expr Poly.Type)

data Definition = Definition (Metadata SourceInfo) Identifier CanonicalExpr
                | ForeignDefinition (Metadata SourceInfo) Identifier Poly.Scheme
                | TypeDefinition (Metadata SourceInfo) QualifiedName [NameBinding] Poly.Type
                deriving (Show, Eq, Ord)

type PackageName = [String]

data PackageDeclaration = PackageDeclaration (Metadata SourceInfo) PackageName
                        deriving (Show, Eq, Ord)

data ImportedPackage = ImportedPackage (Metadata SourceInfo) Identifier Package
                     deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [ImportedPackage] [Definition]
             deriving (Show, Eq, Ord)
