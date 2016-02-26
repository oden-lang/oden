module Oden.Infer.Environment where

import Oden.Identifier
import qualified Oden.Core as Core
import Oden.Environment
import Oden.QualifiedName (QualifiedName(..))
import Oden.SourceInfo
import Oden.Type.Polymorphic

import qualified Data.Map as Map

data TypeBinding = Package SourceInfo Name TypingEnvironment
                 | Local SourceInfo Name Scheme
                 | Struct SourceInfo QualifiedName [Core.NameBinding] [Core.StructField Type]
                 deriving (Show, Eq)

type TypingEnvironment = Environment TypeBinding

fromDefinitions :: Map.Map Name Core.Definition -> TypingEnvironment
fromDefinitions defs = Environment (Map.map convert defs)
  where convert (Core.Definition si n (sc, _)) = Local si n sc
        convert (Core.ForeignDefinition si n sc) = Local si n sc
        convert (Core.StructDefinition si n bindings fields) =
          Struct si n bindings fields
