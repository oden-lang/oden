-- | The Environment is a representation of the bindings and protocol
-- implementations visible in a certain scope, e.g. a package scope with
-- imports, top-level definitions and local bindings.
module Oden.Environment where

import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Prelude         hiding (lookup)

import           Oden.Identifier

data Environment b i
  = Environment { environmentBindings :: Map.Map Identifier b
                , environmentImplementations :: Set i
                }
  deriving (Show, Eq)

empty :: Environment b i
empty = Environment Map.empty Set.empty

assocs :: Environment b i -> [(Identifier, b)]
assocs (Environment m _) = Map.assocs m

bindings :: Environment b i -> [b]
bindings (Environment m _) = Map.elems m

implementations :: Environment b i -> Set i
implementations (Environment _ impls) = impls

lookup :: Identifier -> Environment b i -> Maybe b
lookup i (Environment m _) = Map.lookup i m

map :: (a -> b) -> Environment a i -> Environment b i
map f (Environment e impls) = Environment (Map.map f e) impls

insert :: Identifier -> b -> Environment b i -> Environment b i
insert i binding (Environment m impls) =
  Environment (Map.insert i binding m) impls

extend :: Environment b i -> (Identifier, b) -> Environment b i
extend (Environment m impls) (identifier, binding) =
  Environment (Map.insert identifier binding m) impls

addImplementation :: Ord i => i -> Environment b i -> Environment b i
addImplementation impl (Environment bindings' impls) =
  Environment bindings' (impl `Set.insert` impls)

removeImplementation :: Ord i => i -> Environment b i -> Environment b i
removeImplementation impl (Environment bindings' impls) =
  Environment bindings' (impl `Set.delete` impls)

merge :: Ord i => Environment b i -> Environment b i -> Environment b i
merge (Environment m1 i1) (Environment m2 i2) = Environment (m1 `Map.union` m2) (i1 `Set.union` i2)

fromList :: Ord i => [(Identifier, b)] -> Environment b i
fromList = flip fromLists []

fromLists :: Ord i => [(Identifier, b)] -> [i] -> Environment b i
fromLists bindings' impls = Environment (Map.fromList bindings') (Set.fromList impls)

singleton :: Identifier -> b -> Environment b i
singleton i binding = Environment (Map.singleton i binding) Set.empty

singletonImplementation :: i -> Environment b i
singletonImplementation impl = Environment Map.empty (Set.singleton impl)
