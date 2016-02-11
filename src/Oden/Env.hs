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
  fromScope,
) where

import           Prelude               hiding (lookup)

import           Oden.Identifier
import qualified Oden.Scope as Scope
import           Oden.Type.Polymorphic

import           Data.Foldable         hiding (toList)
import qualified Data.Map              as Map

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

fromScope :: Scope.Scope -> Env
fromScope scope = fromList as
  where toEnvAssoc (_, i, Scope.ForeignDefinition _ s) = (i, s)
        toEnvAssoc (_, i, Scope.OdenDefinition _ s _ _) = (i, s)
        as = map toEnvAssoc (Scope.assocs scope)
