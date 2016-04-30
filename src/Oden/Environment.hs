-- | The Environment is a representation of the bindings and protocol
-- implementations visible in a certain scope, e.g. a package scope with
-- imports, top-level definitions and local bindings.
module Oden.Environment where

import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude               hiding (lookup)

import           Oden.Identifier

data Environment b i
  = Environment (Map.Map Identifier b) [i]
  deriving (Show, Eq)

empty :: Environment b i
empty = Environment Map.empty []

assocs :: Environment b i -> [(Identifier, b)]
assocs (Environment m _) = Map.assocs m

bindings :: Environment b i -> [b]
bindings (Environment m _) = Map.elems m

implementations :: Environment b i -> [i]
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

addImplementation :: Environment b i -> i -> Environment b i
addImplementation (Environment bindings impls) impl =
  Environment bindings (impls ++ [impl])

merge :: Environment b i -> Environment b i -> Environment b i
merge (Environment m1 i1) (Environment m2 i2) = Environment (m1 `Map.union` m2) (i1 ++ i2)

fromList :: [(Identifier, b)] -> Environment b i
fromList = flip fromLists []

fromLists :: [(Identifier, b)] -> [i] -> Environment b i
fromLists = Environment . Map.fromList

singleton :: Identifier -> b -> Environment b i
singleton i binding = Environment (Map.singleton i binding) []

singletonImplementation :: i -> Environment b i
singletonImplementation impl = Environment Map.empty [impl]
