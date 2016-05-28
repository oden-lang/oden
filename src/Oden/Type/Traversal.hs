-- | Traverses and transforms a 'Type' recursively by applying a supplied
-- monadic function for each found 'Type'.

module Oden.Type.Traversal where

import           Oden.Type.Polymorphic

traverseType :: Monad m
             => (Type -> m Type)
             -> Type
             -> m Type
traverseType onType' type' = do
  let recurse = traverseType onType'
  case type' of
    t@TVar{} ->
      onType' t
    TTuple si f s r ->
      TTuple si <$> recurse f <*> recurse s <*> mapM recurse r >>= onType'
    TApp si f p ->
      TApp si <$> recurse f <*> recurse p >>= onType'
    TFn si p r ->
      TFn si <$> recurse p <*> recurse r >>= onType'
    TNoArgFn si r ->
      TNoArgFn si <$> recurse r >>= onType'
    t@TCon{} ->
      onType' t
    TSlice si t ->
      TSlice si <$> recurse t >>= onType'
    TRecord si fs ->
      TRecord si <$> recurse fs >>= onType'
    TNamed si n t ->
      TNamed si n <$> recurse t >>= onType'
    TConstrained cs t ->
      TConstrained cs <$> recurse t >>= onType'
    t@REmpty{} -> onType' t
    RExtension si l t r ->
      RExtension si l <$> recurse t <*> recurse r >>= onType'
    TForeignFn si v p r  ->
      TForeignFn si v <$> mapM recurse p <*> mapM recurse r >>= onType'
