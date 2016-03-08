module Oden.Infer.Environment (
  TypeBinding(..),
  TypingEnvironment,
  fromPackage,
  fromPackages
) where

import Oden.Identifier
import qualified Oden.Core as Core
import Oden.Environment hiding (map)
import Oden.QualifiedName (QualifiedName(..))
import Oden.SourceInfo
import Oden.Type.Polymorphic

data TypeBinding = Package SourceInfo Identifier TypingEnvironment
                 | Local SourceInfo Identifier Scheme
                 | Type SourceInfo QualifiedName [Core.NameBinding] Type
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
