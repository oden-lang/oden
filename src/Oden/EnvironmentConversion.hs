module Oden.EnvironmentConversion where

import qualified Oden.Core as Core
import Oden.Environment
import Oden.Compiler.Environment as Compiler
import Oden.Infer.Environment as Infer

import qualified Data.Map as Map

asTypingEnvironment :: CompileEnvironment -> TypingEnvironment
asTypingEnvironment (Environment m) = Environment $ Map.map f m
  where
  f :: Binding -> TypeBinding
  f (Compiler.Package si n e) =
    Infer.Package si n (asTypingEnvironment e)
  f (Compiler.Definition (Core.Definition si n (sc, _))) =
    Infer.Local si n sc
  f (Compiler.Definition (Core.ForeignDefinition si n sc)) =
    Infer.Local si n sc
