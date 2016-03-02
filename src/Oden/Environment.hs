module Oden.Environment where

import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude               hiding (lookup)

import           Oden.Identifier

newtype Environment b = Environment (Map.Map Identifier b) deriving (Show, Eq)

empty :: Environment b
empty = Environment Map.empty

assocs :: Environment b -> [(Identifier, b)]
assocs (Environment m) = Map.assocs m

lookup :: Identifier -> Environment b -> Maybe b
lookup i (Environment m) = Map.lookup i m

map :: (a -> b) -> Environment a -> Environment b
map f (Environment e) = Environment (Map.map f e)

insert :: Identifier -> b -> Environment b -> Environment b
insert i binding (Environment m) =
  Environment (Map.insert i binding m)

extend :: Environment b -> (Identifier, b) -> Environment b
extend (Environment m) (identifier, binding) =
  Environment (Map.insert identifier binding m)

merge :: Environment b -> Environment b -> Environment b
merge (Environment m1) (Environment m2) = Environment (m1 `Map.union` m2)

fromList :: [(Identifier, b)] -> Environment b
fromList = Environment . Map.fromList
