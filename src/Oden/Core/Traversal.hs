-- | Traverses and transforms an 'Expr' recursively by applying a supplied
-- monadic functions for each found 'Expr'.

module Oden.Core.Traversal where

import           Oden.Core.Expr

data Traversal m t a = Traversal { onType :: t -> m t
                                 , onMemberAccess :: a -> m a }

traverseExpr :: Monad m
              => Traversal m t a
              -> Expr r t a
              -> m (Expr r t a)
traverseExpr traversal@Traversal{ onType = onType' } type' = do
  let recurse = traverseExpr traversal
  case type' of
    Symbol si identifier t ->
      Symbol si identifier <$> onType' t
    Subscript si expr i t ->
      Subscript si <$> recurse expr <*> return i <*> onType' t
    Subslice si expr range t ->
      Subslice si <$> recurse expr <*> return range <*> onType' t
    UnaryOp si op expr t ->
      UnaryOp si op <$> recurse expr <*> onType' t
    BinaryOp si op lhs rhs t ->
      BinaryOp si op <$> recurse lhs <*> recurse rhs <*> onType' t
    Application si f arg t ->
      Application si <$> recurse f <*> recurse arg <*> onType' t
    NoArgApplication si f t ->
      NoArgApplication si <$> recurse f  <*> onType' t
    ForeignFnApplication si f args t ->
      ForeignFnApplication si <$> recurse f <*> mapM recurse args <*> onType' t
    Fn si param body t ->
      Fn si param <$> recurse body <*> onType' t
    NoArgFn si body t ->
      NoArgFn si <$> recurse body <*> onType' t
    Let si binding value body t ->
      Let si binding <$> recurse value <*> recurse body <*> onType' t
    Literal si literal t ->
      Literal si literal <$> onType' t
    Tuple si f s r t ->
      Tuple si <$> recurse f <*> recurse s <*> mapM recurse r <*> onType' t
    Slice si exprs t ->
      Slice si <$> mapM recurse exprs <*> onType' t
    If si condition thenBranch elseBranch t ->
      If si <$> recurse condition
            <*> recurse thenBranch
            <*> recurse elseBranch
            <*> onType' t
    Block si exprs t ->
      Block si <$> mapM recurse exprs <*> onType' t
    RecordInitializer si fields t ->
      RecordInitializer si <$> mapM onField fields <*> onType' t
      where
      onField (FieldInitializer fsi fieldName fieldValue) =
        FieldInitializer fsi fieldName <$> recurse fieldValue
    MemberAccess si access t ->
      MemberAccess si <$> onMemberAccess traversal access <*> onType' t
    MethodReference si ref t ->
      MethodReference si ref <$> onType' t
