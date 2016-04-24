module Oden.Infer.Environment (
  TypeBinding(..),
  TypingEnvironment,
  fromPackage,
  fromPackages
) where

import           Oden.Core
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Environment      hiding (map)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName    (QualifiedName (..))
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data TypeBinding = PackageBinding (Metadata SourceInfo) Identifier TypingEnvironment
                 | Local (Metadata SourceInfo) Identifier Scheme
                 | Type (Metadata SourceInfo) QualifiedName [NameBinding] Type
                 | QuantifiedType (Metadata SourceInfo) Identifier Type
                 | ProtocolBinding (Metadata SourceInfo) Identifier Protocol
                 deriving (Show, Eq)

type TypingEnvironment = Environment TypeBinding

fromPackage :: TypedPackage -> TypingEnvironment
fromPackage (Package _ _ defs) =
  fromList (map convert defs)
  where
  convert (Definition si n (sc, _)) = (n, Local si n sc)
  convert (ForeignDefinition si n sc) = (n, Local si n sc)
  convert (TypeDefinition si qualified@(FQN _ n) bs t) = (n, Type si qualified bs t)
  convert (ProtocolDefinition si (FQN _ name) protocol) =
    (name, ProtocolBinding si name protocol)

fromPackages :: [ImportedPackage] -> TypingEnvironment
fromPackages =
  foldl iter empty
  where
  iter env (ImportedPackage sourceInfo pkgIdentifier pkg) =
    env `extend` (pkgIdentifier, PackageBinding sourceInfo pkgIdentifier $ fromPackage pkg)
