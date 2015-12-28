module Oden.Env (
  Env(..),
  empty,
  lookup,
  remove,
  extend,
  extends,
  merge,
  mergeEnvs,
  singleton,
  keys,
  fromList,
  toList,
) where

import Prelude hiding (lookup)

import Oden.Identifier
import Oden.Syntax
import Oden.Type.Polymorphic

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

data Env = TypeEnv { types :: Map.Map Identifier Scheme }
  deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Identifier, Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> Identifier -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Identifier, Scheme)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: Identifier -> Env -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Identifier -> Scheme -> Env
singleton x y = TypeEnv (Map.singleton x y)

keys :: Env -> [Identifier]
keys (TypeEnv env) = Map.keys env

fromList :: [(Identifier, Scheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(Identifier, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Monoid Env where
  mempty = empty
  mappend = merge
