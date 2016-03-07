module Oden.Syntax where

import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.SourceInfo
import           Oden.Type.Signature

data NameBinding = NameBinding SourceInfo Identifier
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
          | StructInitializer SourceInfo SignatureExpr [Expr]
          | MemberAccess SourceInfo Expr Expr
          deriving (Show, Eq, Ord)

instance HasSourceInfo Expr where
  getSourceInfo (Symbol si _)              = si
  getSourceInfo (Subscript si _ _)         = si
  getSourceInfo (UnaryOp si _ _)           = si
  getSourceInfo (BinaryOp si _ _ _)        = si
  getSourceInfo (Application si _ _)       = si
  getSourceInfo (Fn si _ _)                = si
  getSourceInfo (Let si _ _)               = si
  getSourceInfo (Literal si _)             = si
  getSourceInfo (If si _ _ _)              = si
  getSourceInfo (Slice si _)               = si
  getSourceInfo (Tuple si _ _ _)           = si
  getSourceInfo (Block si _)               = si
  getSourceInfo (StructInitializer si _ _) = si
  getSourceInfo (MemberAccess si _ _)      = si

  setSourceInfo si (Symbol _ i)                   = Symbol si i
  setSourceInfo si (Subscript _ s i)              = Subscript si s i
  setSourceInfo si (UnaryOp _ o r)                = UnaryOp si o r
  setSourceInfo si (BinaryOp _ p l r)             = BinaryOp si p l r
  setSourceInfo si (Application _ f a)            = Application si f a
  setSourceInfo si (Fn _ n b)                     = Fn si n b
  setSourceInfo si (Let _ p b)                    = Let si p b
  setSourceInfo si (Literal _ l)                  = Literal si l
  setSourceInfo si (If _ c t e)                   = If si c t e
  setSourceInfo si (Slice _ e)                    = Slice si e
  setSourceInfo si (Tuple _ f s r)                = Tuple si f s r
  setSourceInfo si (Block _ e)                    = Block si e
  setSourceInfo si (StructInitializer _ e vs)     = StructInitializer si e vs
  setSourceInfo si (MemberAccess _ pkgAlias name) = MemberAccess si pkgAlias name

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

type PackageName = [String]

data PackageDeclaration = PackageDeclaration SourceInfo PackageName
                          deriving (Show, Eq, Ord)

data StructFieldExpr = StructFieldExpr SourceInfo Identifier SignatureExpr
                     deriving (Show, Eq, Ord)

data TopLevel = ImportDeclaration SourceInfo PackageName
              | TypeSignatureDeclaration SourceInfo Identifier TypeSignature
              | ValueDefinition SourceInfo Identifier Expr
              | FnDefinition SourceInfo Identifier [NameBinding] Expr
              -- TODO: Add support for type parameters
              | TypeDefinition SourceInfo Identifier SignatureExpr
              deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [TopLevel]
             deriving (Show, Eq, Ord)
