-- | This module containts the Oden AST - the representation of the syntax.
{-# LANGUAGE LambdaCase #-}
module Oden.Syntax where

import           Oden.Identifier
import           Oden.SourceInfo
import           Oden.Type.Signature

data NameBinding = NameBinding SourceInfo Identifier
                 deriving (Show, Eq, Ord)

data LetPair = LetPair SourceInfo NameBinding Expr
             deriving (Show, Eq, Ord)

data FieldInitializer = FieldInitializer SourceInfo Identifier Expr
                      deriving (Show, Eq, Ord)

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | EqualTo
  | NotEqualTo
  | MonoidApply
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | And
  | Or
  deriving (Show, Eq, Ord)

data UnaryOperator
  = Negate
  | Not
  | Go
  | Receive
  deriving (Show, Eq, Ord)

data Expr
  = Symbol SourceInfo Identifier
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
  | RecordInitializer SourceInfo [FieldInitializer]
  | MemberAccess SourceInfo Expr Expr
  | ProtocolMethodReference SourceInfo Expr Expr
  deriving (Show, Eq, Ord)

instance HasSourceInfo Expr where
  getSourceInfo =
    \case
      Symbol si _                    -> si
      Subscript si _ _               -> si
      UnaryOp si _ _                 -> si
      BinaryOp si _ _ _              -> si
      Application si _ _             -> si
      Fn si _ _                      -> si
      Let si _ _                     -> si
      Literal si _                   -> si
      If si _ _ _                    -> si
      Slice si _                     -> si
      Tuple si _ _ _                 -> si
      Block si _                     -> si
      RecordInitializer si _         -> si
      MemberAccess si _ _            -> si
      ProtocolMethodReference si _ _ -> si

  setSourceInfo si =
    \case
      Symbol _ i                                -> Symbol si i
      Subscript _ s i                           -> Subscript si s i
      UnaryOp _ o r                             -> UnaryOp si o r
      BinaryOp _ p l r                          -> BinaryOp si p l r
      Application _ f a                         -> Application si f a
      Fn _ n b                                  -> Fn si n b
      Let _ p b                                 -> Let si p b
      Literal _ l                               -> Literal si l
      If _ c t e                                -> If si c t e
      Slice _ e                                 -> Slice si e
      Tuple _ f s r                             -> Tuple si f s r
      Block _ e                                 -> Block si e
      RecordInitializer _ fs                    -> RecordInitializer si fs
      MemberAccess _ pkgAlias name              -> MemberAccess si pkgAlias name
      ProtocolMethodReference _ protocol method -> ProtocolMethodReference si protocol method

data Subscript = Singular Expr
               | RangeTo Expr
               | RangeFrom Expr
               | Range Expr Expr
               deriving (Show, Eq, Ord)

data Literal = Int Integer
             | Float Double
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

type PackageName = [String]

data PackageDeclaration = PackageDeclaration SourceInfo PackageName
                          deriving (Show, Eq, Ord)

data RecordFieldExpr = RecordFieldExpr SourceInfo Identifier SignatureExpr
                     deriving (Show, Eq, Ord)
data Definition
  = ValueDefinition SourceInfo Identifier Expr
  | FnDefinition SourceInfo Identifier [NameBinding] Expr
  deriving (Show, Eq, Ord)

data TopLevel
  = ImportDeclaration SourceInfo [String]
  | ImportForeignDeclaration SourceInfo String
  | TypeSignatureDeclaration SourceInfo Identifier TypeSignature
  | TopLevelDefinition Definition
  -- TODO: Add support for type parameters
  | TypeDefinition SourceInfo Identifier SignatureExpr
  | ProtocolDefinition SourceInfo Identifier SignatureVarBinding [ProtocolMethodSignature]
  | Implementation SourceInfo TypeSignature [Definition]
  deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [TopLevel]
             deriving (Show, Eq, Ord)
