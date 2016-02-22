module Oden.Infer.Environment where

import Oden.Identifier
import Oden.Type.Polymorphic
import Oden.SourceInfo
import qualified Oden.Core as Core
import Oden.Environment

import qualified Data.Map as Map

data TypeBinding = Package SourceInfo Name TypingEnvironment
                 | Local SourceInfo Name Scheme
                 | TypeAlias SourceInfo Name [TVarBinding] Type
                 deriving (Show, Eq)

type TypingEnvironment = Environment TypeBinding

fromDefinitions :: Map.Map Name Core.Definition -> TypingEnvironment
fromDefinitions defs = Environment (Map.map convert defs)
  where convert (Core.Definition si n (sc, _)) = Local si n sc
        convert (Core.ForeignDefinition si n sc) = Local si n sc
