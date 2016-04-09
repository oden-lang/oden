module Oden.Core.Untyped where

import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName (QualifiedName(..))
import           Oden.SourceInfo
import           Oden.Type.Signature

data NameBinding = NameBinding (Metadata SourceInfo) Identifier
                 deriving (Show, Eq, Ord)

data FieldInitializer = FieldInitializer (Metadata SourceInfo) Identifier Expr
                        deriving (Show, Eq, Ord)

data Expr = Symbol (Metadata SourceInfo) Identifier
          | Subscript (Metadata SourceInfo) Expr Expr
          | Subslice (Metadata SourceInfo) Expr Range
          | UnaryOp (Metadata SourceInfo) UnaryOperator Expr
          | BinaryOp (Metadata SourceInfo) BinaryOperator Expr Expr
          | Application (Metadata SourceInfo) Expr [Expr]
          | Fn (Metadata SourceInfo) NameBinding Expr
          | NoArgFn (Metadata SourceInfo) Expr
          | Let (Metadata SourceInfo) NameBinding Expr Expr
          | Literal (Metadata SourceInfo) Literal
          | Tuple (Metadata SourceInfo) Expr Expr [Expr]
          | If (Metadata SourceInfo) Expr Expr Expr
          | Slice (Metadata SourceInfo) [Expr]
          | Block (Metadata SourceInfo) [Expr]
          | RecordInitializer (Metadata SourceInfo) [FieldInitializer]
          | MemberAccess (Metadata SourceInfo) Expr Identifier
          deriving (Show, Eq, Ord)

instance HasSourceInfo Expr where
  getSourceInfo (Symbol (Metadata si) _)                   = si
  getSourceInfo (Subscript (Metadata si) _ _)              = si
  getSourceInfo (Subslice (Metadata si) _ _)               = si
  getSourceInfo (UnaryOp (Metadata si) _ _)                = si
  getSourceInfo (BinaryOp (Metadata si) _ _ _)             = si
  getSourceInfo (Application (Metadata si) _ _)            = si
  getSourceInfo (Fn (Metadata si) _ _)                     = si
  getSourceInfo (NoArgFn (Metadata si) _)                  = si
  getSourceInfo (Let (Metadata si) _ _ _)                  = si
  getSourceInfo (Literal (Metadata si) _)                  = si
  getSourceInfo (If (Metadata si) _ _ _)                   = si
  getSourceInfo (Slice (Metadata si) _)                    = si
  getSourceInfo (Tuple (Metadata si) _ _ _)                = si
  getSourceInfo (Block (Metadata si) _)                    = si
  getSourceInfo (RecordInitializer (Metadata si) _)        = si
  getSourceInfo (MemberAccess (Metadata si) _ _)           = si

  setSourceInfo si (Symbol _ i)                   = Symbol (Metadata si) i
  setSourceInfo si (Subscript _ s i)              = Subscript (Metadata si) s i
  setSourceInfo si (Subslice _ s r)               = Subslice (Metadata si) s r
  setSourceInfo si (UnaryOp _ p r)                = UnaryOp (Metadata si) p r
  setSourceInfo si (BinaryOp _ p l r)             = BinaryOp (Metadata si) p l r
  setSourceInfo si (Application _ f a)            = Application (Metadata si) f a
  setSourceInfo si (Fn _ n b)                     = Fn (Metadata si) n b
  setSourceInfo si (NoArgFn _ b)                  = NoArgFn (Metadata si) b
  setSourceInfo si (Let _ n v b)                  = Let (Metadata si) n v b
  setSourceInfo si (Literal _ l)                  = Literal (Metadata si) l
  setSourceInfo si (If _ c t e)                   = If (Metadata si) c t e
  setSourceInfo si (Slice _ e)                    = Slice (Metadata si) e
  setSourceInfo si (Tuple _ f s r)                = Tuple (Metadata si) f s r
  setSourceInfo si (Block _ e)                    = Block (Metadata si) e
  setSourceInfo si (RecordInitializer _ fs)       = RecordInitializer (Metadata si) fs
  setSourceInfo si (MemberAccess _ expr n)        = MemberAccess (Metadata si) expr n

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

data Range = Range Expr Expr
           | RangeTo Expr
           | RangeFrom Expr
           deriving (Show, Eq, Ord)

data RecordField = RecordField (Metadata SourceInfo) Identifier (SignatureExpr SourceInfo)
                 deriving (Show, Eq, Ord)

data Definition = Definition (Metadata SourceInfo) Identifier (Maybe (TypeSignature SourceInfo)) Expr
                | TypeDefinition (Metadata SourceInfo) QualifiedName [NameBinding] (SignatureExpr SourceInfo)
                | ProtocolDefinition (Metadata SourceInfo) QualifiedName (SignatureVarBinding SourceInfo) [ProtocolMethodSignature SourceInfo]
                deriving (Show, Eq, Ord)

type PackageName = [String]

data PackageDeclaration = PackageDeclaration (Metadata SourceInfo) PackageName
                          deriving (Show, Eq, Ord)

data ImportReference = ImportReference (Metadata SourceInfo) PackageName
                     deriving (Show, Eq, Ord)

data Package i = Package PackageDeclaration i [Definition]
             deriving (Show, Eq, Ord)
