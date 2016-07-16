module Oden.Core.Untyped where

import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName (QualifiedName(..))
import           Oden.SourceInfo
import           Oden.Type.Signature

data Untyped = Untyped

instance Show Untyped where
  show _ = "<untyped>"

instance Eq Untyped where
  _ == _ = True

instance Ord Untyped where
  compare _ _ = EQ

data NamedMemberAccess = NamedMemberAccess UntypedExpr Identifier
                       deriving (Show, Eq, Ord)

data NamedMethodReference = NamedMethodReference Identifier Identifier
                          deriving (Show, Eq, Ord)

type UntypedExpr = Expr NamedMethodReference Untyped NamedMemberAccess

data MethodImplementation
  = MethodImplementation (Metadata SourceInfo) Identifier UntypedExpr
  deriving (Show, Eq, Ord)

data Definition
  = Definition (Metadata SourceInfo) Identifier (Maybe TypeSignature) UntypedExpr
  | TypeDefinition (Metadata SourceInfo) QualifiedName [NameBinding] SignatureExpr
  | ProtocolDefinition (Metadata SourceInfo) QualifiedName SignatureVarBinding [ProtocolMethodSignature]
  | Implementation (Metadata SourceInfo) TypeSignature [MethodImplementation]
  deriving (Show, Eq, Ord)

data UntypedPackage i = UntypedPackage PackageDeclaration [i] [Definition]
  deriving (Show, Eq, Ord)
