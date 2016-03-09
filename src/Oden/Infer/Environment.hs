module Oden.Infer.Environment (
  TypeBinding(..),
  TypingEnvironment,
  fromPackage,
  fromPackages
) where

import Oden.Identifier
import qualified Oden.Core as Core
import Oden.Environment hiding (map)
import Oden.Metadata
import Oden.QualifiedName (QualifiedName(..))
import Oden.SourceInfo
import Oden.Type.Polymorphic

data TypeBinding = Package (Metadata SourceInfo) Identifier TypingEnvironment
                 | Local (Metadata SourceInfo) Identifier Scheme
                 | Type (Metadata SourceInfo) QualifiedName [Core.NameBinding] Type
                 | QuantifiedType (Metadata SourceInfo) Identifier Type
                 deriving (Show, Eq)

type TypingEnvironment = Environment TypeBinding

fromPackage :: Core.Package -> TypingEnvironment
fromPackage (Core.Package _ _ defs) =
  fromList (map convert defs)
  where
  convert (Core.Definition si n (sc, _)) = (n, Local si n sc)
  convert (Core.ForeignDefinition si n sc) = (n, Local si n sc)
  convert (Core.TypeDefinition si qualified@(FQN _ n) bs t) = (n, Type si qualified bs t)

fromPackages :: [Core.ImportedPackage] -> TypingEnvironment
fromPackages =
  foldl iter empty
  where
  iter env (Core.ImportedPackage sourceInfo pkgIdentifier pkg) =
    env `extend` (pkgIdentifier, Package sourceInfo pkgIdentifier $ fromPackage pkg)
