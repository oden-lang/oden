{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE LambdaCase       #-}

-- | Collects all ProtocolConstraints, removing TConstrained recursively in
-- the tree.
module Oden.Infer.ConstraintCollection (collectConstraints) where

import           Control.Monad.Writer

import           Oden.Core.Typed
import           Oden.Core.Traversal
import           Oden.Type.Polymorphic
import           Oden.Type.Traversal

import Data.Set as Set


collectConstraints :: TypedExpr -> Set ProtocolConstraint
collectConstraints = execWriter . traverseExpr traversal
  where
  traversal = identityTraversal { onType = traverseType onType'
                                , onMethodReference = onMethodReference'
                                , onMemberAccess = return
                                }
    where
    onType' t@(TConstrained cs _) = do
      tell (Set.filter isTypeVarConstraint cs)
      return t
    onType' t = return t

    onMethodReference' si reference type' = (si, reference,) <$> onType' type'

    isTypeVarConstraint =
      \case
        ProtocolConstraint _ _ TVar{} -> True
        _                             -> False
