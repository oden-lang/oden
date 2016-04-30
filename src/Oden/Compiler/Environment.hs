module Oden.Compiler.Environment where

import           Oden.Core.Resolved
import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Environment hiding (map)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo

data Binding = PackageBinding (Metadata SourceInfo) Identifier (Environment Binding)
             | DefinitionBinding ResolvedDefinition
             | LetBinding NameBinding ResolvedExpr
             | FunctionArgument NameBinding
             deriving (Show, Eq)

type CompileEnvironment = Environment Binding

fromPackage :: ResolvedPackage -> CompileEnvironment
fromPackage (ResolvedPackage _ _ defs) =
  fromList (concatMap convert defs)
  where
  convert d@(Definition _ n _) = [(n, DefinitionBinding d)]
  convert d@(ForeignDefinition _ n _) = [(n, DefinitionBinding d)]
  -- All type and protocol usage should already be resolved before the
  -- compilation phase so we can safely ignore these.
  convert TypeDefinition{} = []
  convert ProtocolDefinition{} = []

fromPackages :: [ImportedPackage ResolvedPackage] -> CompileEnvironment
fromPackages =
  foldl iter empty
  where
  iter env (ImportedPackage sourceInfo pkgIdentifier pkg) =
    env `extend` (pkgIdentifier, PackageBinding sourceInfo pkgIdentifier $ fromPackage pkg)
