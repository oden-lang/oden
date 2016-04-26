-- | Traverses and transforms an 'Expr' recursively by applying a supplied
-- monadic functions for each found 'Expr'.

{-# LANGUAGE LambdaCase #-}
module Oden.Core.Traversal where

import           Oden.Core.Expr

import           Data.Maybe

data Traversal m r t a = Traversal { onExpr :: Expr r t a -> m (Maybe (Expr r t a))
                                   , onType :: t -> m t
                                   , onMemberAccess :: a -> m a
                                   , onNameBinding :: NameBinding -> m NameBinding }

identityTraversal :: Monad m => Traversal m r t a
identityTraversal = Traversal { onExpr = const (return Nothing)
                              , onType = return
                              , onMemberAccess = return
                              , onNameBinding = return }

traverseExpr :: Monad m
              => Traversal m r t a
              -> Expr r t a
              -> m (Expr r t a)
traverseExpr traversal@Traversal{ onExpr = onExpr'
                                , onType = onType'
                                , onNameBinding = onNameBinding' } = traverseExpr'
  where
  traverseExpr' expr =
    fromMaybe <$> traverseDefault expr <*> onExpr' expr

  traverseDefault = \case
    Symbol si identifier t ->
      Symbol si identifier <$> onType' t
    Subscript si slice i t ->
      Subscript si <$> traverseExpr' slice <*> return i <*> onType' t
    Subslice si slice range t ->
      Subslice si <$> traverseExpr' slice <*> return range <*> onType' t
    UnaryOp si operator operand t ->
      UnaryOp si operator <$> traverseExpr' operand <*> onType' t
    BinaryOp si op lhs rhs t ->
      BinaryOp si op <$> traverseExpr' lhs <*> traverseExpr' rhs <*> onType' t
    Application si f arg t ->
      Application si <$> traverseExpr' f <*> traverseExpr' arg <*> onType' t
    NoArgApplication si f t ->
      NoArgApplication si <$> traverseExpr' f  <*> onType' t
    ForeignFnApplication si f args t ->
      ForeignFnApplication si <$> traverseExpr' f <*> mapM traverseExpr' args <*> onType' t
    Fn si param body t ->
      Fn si <$> onNameBinding' param <*> traverseExpr' body <*> onType' t
    NoArgFn si body t ->
      NoArgFn si <$> traverseExpr' body <*> onType' t
    Let si binding value body t ->
      Let si <$> onNameBinding' binding <*> traverseExpr' value <*> traverseExpr' body <*> onType' t
    Literal si literal t ->
      Literal si literal <$> onType' t
    Tuple si f s r t ->
      Tuple si <$> traverseExpr' f <*> traverseExpr' s <*> mapM traverseExpr' r <*> onType' t
    Slice si exprs t ->
      Slice si <$> mapM traverseExpr' exprs <*> onType' t
    If si condition thenBranch elseBranch t ->
      If si <$> traverseExpr' condition
            <*> traverseExpr' thenBranch
            <*> traverseExpr' elseBranch
            <*> onType' t
    Block si exprs t ->
      Block si <$> mapM traverseExpr' exprs <*> onType' t
    RecordInitializer si fields t ->
      RecordInitializer si <$> mapM onField fields <*> onType' t
      where
      onField (FieldInitializer fsi fieldName fieldValue) =
        FieldInitializer fsi fieldName <$> traverseExpr' fieldValue
    MemberAccess si access t ->
      MemberAccess si <$> onMemberAccess traversal access <*> onType' t
    MethodReference si ref t ->
      MethodReference si ref <$> onType' t
