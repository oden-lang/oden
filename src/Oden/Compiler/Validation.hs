module Oden.Compiler.Validation where

import           Oden.Core
import           Oden.Identifier

import           Control.Monad
import qualified Data.Set        as Set

data ValidationError = Redefinition Name
                     deriving (Show, Eq, Ord)

data ValidationWarning = NameShadowed Name
                       deriving (Show, Eq, Ord)

checkRedefinitions :: Package -> Either ValidationError [ValidationWarning]
checkRedefinitions _p@(Package _name _imports definitions) = do
  _ <- foldM iter Set.empty definitions
  return []
  where
  iter names (Definition name _)
    | Set.member name names = Left (Redefinition name)
    | otherwise = return (Set.insert name names)

runAll :: Package -> Either [ValidationError] [ValidationWarning]
runAll pkg = case checkRedefinitions pkg of
  Left err -> Left [err]
  Right warnings -> return warnings
