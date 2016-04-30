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

data Definition
  = Definition (Metadata SourceInfo) Identifier (Maybe (TypeSignature SourceInfo)) UntypedExpr
  | TypeDefinition (Metadata SourceInfo) QualifiedName [NameBinding] (SignatureExpr SourceInfo)
  | ProtocolDefinition (Metadata SourceInfo) QualifiedName (SignatureVarBinding SourceInfo) [ProtocolMethodSignature SourceInfo]
  deriving (Show, Eq, Ord)

data UntypedPackage i = UntypedPackage PackageDeclaration [i] [Definition]
