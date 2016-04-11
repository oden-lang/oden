module Oden.Compiler.Environment where

import qualified Oden.Core as Core
import           Oden.Core.Expr
import           Oden.Environment hiding (map)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo

data Binding = Package (Metadata SourceInfo) Identifier (Environment Binding)
             | Definition Core.Definition
             | LetBinding NameBinding Core.TypedExpr
             | FunctionArgument NameBinding
             deriving (Show, Eq)

type CompileEnvironment = Environment Binding

fromPackage :: Core.Package -> CompileEnvironment
fromPackage (Core.Package _ _ defs) =
  fromList (concatMap convert defs)
  where
  convert d@(Core.Definition _ n _) = [(n, Definition d)]
  convert d@(Core.ForeignDefinition _ n _) = [(n, Definition d)]
  -- All type and protocol usage should already be resolved before the
  -- compilation phase so we can safely ignore these.
  convert Core.TypeDefinition{} = []
  convert Core.ProtocolDefinition{} = []

fromPackages :: [Core.ImportedPackage] -> CompileEnvironment
fromPackages =
  foldl iter empty
  where
  iter env (Core.ImportedPackage sourceInfo pkgIdentifier pkg) =
    env `extend` (pkgIdentifier, Package sourceInfo pkgIdentifier $ fromPackage pkg)
