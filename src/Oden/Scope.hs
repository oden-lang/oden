module Oden.Scope where

import           Data.List
import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude               hiding (lookup)

import qualified Oden.Core             as Core
import           Oden.Identifier
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic as Poly

data Definition = ForeignDefinition Identifier Poly.Scheme
                | OdenDefinition Identifier Poly.Scheme (Core.Expr Poly.Type) SourceInfo
                deriving (Show, Eq, Ord)

data Source = Predefined
            | Import Core.PackageName
            | Definitions
            deriving (Show, Eq, Ord)

type Layer = Map.Map Identifier Definition

newtype Scope = Scope (Map.Map Source Layer) deriving (Show, Eq)

empty :: Scope
empty = Scope Map.empty

assocs :: Scope -> [(Source, Identifier, Definition)]
assocs (Scope layers) = do
  (source, layer) <- Map.assocs layers
  (i, def) <- Map.assocs layer
  return (source, i, def)

lookup :: Identifier -> Scope -> [(Source, Definition)]
lookup i (Scope layers) = do
  (source, layer) <- Map.assocs layers
  def <- maybeToList (Map.lookup i layer)
  return (source, def)

insert :: Source -> Identifier -> Definition -> Scope -> Scope
insert source i def (Scope layers) =
  let layer = Map.findWithDefault Map.empty source layers
  in Scope (Map.insert source (Map.insert i def layer) layers)

merge :: Scope -> Scope -> Scope
merge s1 s2 = fromList (assocs s1 ++ assocs s2)

fromList :: [(Source, Identifier, Definition)] -> Scope
fromList as = Scope $ foldl iter Map.empty as
  where
  iter layers (source, id', def) =
    let layer = Map.insert id' def (Map.findWithDefault Map.empty source layers)
    in Map.insert source layer layers
