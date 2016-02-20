module Oden.Environment where

import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude               hiding (lookup)

import           Oden.Identifier

newtype Environment b = Environment (Map.Map Name b) deriving (Show, Eq)

empty :: Environment b
empty = Environment Map.empty

assocs :: Environment b -> [(Name, b)]
assocs (Environment m) = Map.assocs m

lookup :: Name -> Environment b -> Maybe b
lookup i (Environment m) = Map.lookup i m

map :: (a -> b) -> Environment a -> Environment b
map f (Environment e) = Environment (Map.map f e)

insert :: Name -> b -> Environment b -> Environment b
insert i binding (Environment m) =
  Environment (Map.insert i binding m)

extend :: Environment b -> (Name, b) -> Environment b
extend (Environment m) (name, binding) =
  Environment (Map.insert name binding m)

merge :: Environment b -> Environment b -> Environment b
merge (Environment m1) (Environment m2) = Environment (m1 `Map.union` m2)

fromList :: [(Name, b)] -> Environment b
fromList = Environment . Map.fromList
