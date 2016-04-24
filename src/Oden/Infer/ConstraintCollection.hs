{-# LANGUAGE FlexibleContexts #-}

-- | Collects all ProtocolConstraints, removing TConstrained recursively in
-- the tree.
module Oden.Infer.ConstraintCollection (collectConstraints) where

import           Control.Monad.Writer

import           Oden.Core
import           Oden.Core.Traversal
import           Oden.Type.Polymorphic
import           Oden.Type.Traversal

import Data.Set

traversal :: Traversal (Writer (Set ProtocolConstraint)) Type TypedMemberAccess
traversal = Traversal { onType = traverseType onType', onMemberAccess = return }
  where
  onType' (TConstrained cs t) = tell cs >> return t
  onType' t = return t

collectConstraints :: TypedExpr -> (TypedExpr, Set ProtocolConstraint)
collectConstraints = runWriter . traverseExpr traversal
