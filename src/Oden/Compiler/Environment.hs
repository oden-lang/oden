module Oden.Compiler.Environment where

import qualified Oden.Core as Core
import Oden.Environment
import           Oden.Identifier
import           Oden.SourceInfo

import qualified Data.Map as Map

data Binding = Package SourceInfo Name (Environment Binding)
             | Definition Core.Definition
             deriving (Show, Eq)

type CompileEnvironment = Environment Binding

fromDefinitions :: Map.Map Name Core.Definition -> CompileEnvironment
fromDefinitions defs = Environment (Map.map Definition defs)
