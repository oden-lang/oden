module Oden.Compiler.Resolution.Environment (
  ResolutionBinding(..),
  ResolutionEnvironment,
  fromPackage
) where

import           Oden.Core
import           Oden.Core.Package
import           Oden.Environment      hiding (map)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo

data ResolutionBinding
  = PackageBinding (Metadata SourceInfo) Identifier ResolutionEnvironment
  | ImplementationBinding (Metadata SourceInfo)
  deriving (Show, Eq)

type ResolutionEnvironment = Environment ResolutionBinding

fromPackage :: TypedPackage -> ResolutionEnvironment
fromPackage (Package _ _ defs) =
  fromList (concatMap convert defs)
  where
  convert _ = []
