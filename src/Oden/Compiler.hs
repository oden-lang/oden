module Oden.Compiler where

import           Oden.Compiler.Monomorphization
import qualified Oden.Core                      as Core

import           Control.Arrow

data CompilationError = MonomorphError MonomorphError
                      deriving (Show, Eq, Ord)

compile :: Core.Package -> Either CompilationError MonomorphedPackage
compile = left MonomorphError . monomorphPackage
