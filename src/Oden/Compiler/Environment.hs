module Oden.Compiler.Environment where

import qualified Oden.Core as Core
import           Oden.Environment hiding (map)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data Binding = Package (Metadata SourceInfo) Identifier (Environment Binding)
             | Definition Core.Definition
             | LetBinding Core.NameBinding (Core.Expr Type)
             | FunctionArgument Core.NameBinding
             deriving (Show, Eq)

type CompileEnvironment = Environment Binding

fromPackage :: Core.Package -> CompileEnvironment
fromPackage (Core.Package _ _ defs) =
  fromList (map convert defs)
  where
  convert d@(Core.Definition _ n _) = (n, Definition d)
  convert d@(Core.ForeignDefinition _ n _) = (n, Definition d)
  convert d@(Core.TypeDefinition _ (FQN _ n) _ _) = (n, Definition d)

fromPackages :: [Core.ImportedPackage] -> CompileEnvironment
fromPackages =
  foldl iter empty
  where
  iter env (Core.ImportedPackage sourceInfo pkgIdentifier pkg) =
    env `extend` (pkgIdentifier, Package sourceInfo pkgIdentifier $ fromPackage pkg)
