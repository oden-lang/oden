module Oden.Compiler.Scope where

import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude               hiding (lookup)

import qualified Oden.Core             as Core
import qualified Oden.Env              as Env
import           Oden.Identifier
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

data Definition = ForeignDefinition Identifier Poly.Type
                | OdenDefinition Identifier Poly.Scheme (Core.Expr Poly.Type)
                deriving (Show, Eq, Ord)


data Source = Predefined
            | Import Core.PackageName
            | Definitions
            deriving (Show, Eq, Ord)

type Layer = Map.Map Identifier Definition

newtype Scope = Scope (Map.Map Source Layer)

lookup :: Identifier -> Scope -> [(Source, Definition)]
lookup i (Scope layers) = do
  (source, layer) <- Map.assocs layers
  def <- maybeToList (Map.lookup i layer)
  return (source, def)

insert :: Source -> Identifier -> Definition -> Scope -> Scope
insert source i def (Scope layers) =
  let layer = Map.findWithDefault Map.empty source layers
  in Scope (Map.insert source (Map.insert i def layer) layers)

envToLayer :: Env.Env -> Layer
envToLayer env = Map.mapWithKey toScope (Env.types env)
  where
  toScope name (Poly.Forall _ t) = ForeignDefinition name t

predefinedEnvToScope :: Env.Env -> Scope
predefinedEnvToScope env = Scope (Map.singleton Predefined $ envToLayer env)
