module Oden.Compiler where

import           Oden.Compiler.Monomorphization
import qualified Oden.Core                      as Core
import           Oden.Scope                     as Scope

import           Control.Arrow

data CompilationError = MonomorphError MonomorphError
                      deriving (Show, Eq, Ord)

compile :: Scope -> Core.Package -> Either CompilationError MonomorphedPackage
compile scope' pkg = left MonomorphError (monomorphPackage scope' pkg)
