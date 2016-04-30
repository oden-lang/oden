{-# LANGUAGE FlexibleContexts #-}

-- | Collects all ProtocolConstraints, removing TConstrained recursively in
-- the tree.
module Oden.Infer.ConstraintCollection (collectConstraints) where

import           Control.Monad.Writer

import           Oden.Core.Typed
import           Oden.Core.Traversal
import           Oden.Type.Polymorphic
import           Oden.Type.Traversal

import Data.Set


collectConstraints :: TypedExpr -> (TypedExpr, Set ProtocolConstraint)
collectConstraints = runWriter . traverseExpr traversal
  where
  traversal = identityTraversal { onType = traverseType onType'
                                , onMethodReference = onMethodReference'
                                , onMemberAccess = return
                                }
    where
    onType' (TConstrained cs t) = tell cs >> return t
    onType' t = return t

    onMethodReference' si reference type' = do
      unconstrainedType <- onType' type'
      return (si, reference, unconstrainedType)
