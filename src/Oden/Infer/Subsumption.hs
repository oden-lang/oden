module Oden.Infer.Subsumption (
  SubsumptionError(..),
  typeSubsumedBy,
  subsumedBy,
  collectSubstitutions
) where

import           Oden.Core.Expr          (typeOf)
import           Oden.Core.Typed         as Typed
import           Oden.Substitution
import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Kind
import           Oden.Type.Polymorphic

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Map                as Map

data SubsumptionError = SubsumptionError SourceInfo Type Type
                      deriving (Show, Eq)

type Subsume a = StateT Subst (Except SubsumptionError) a

-- | Collects the substitutions in the 'Subsume' state for matching types and
-- throws 'SubsumptionError' on mismatches.
collectSubstitutions :: Type -> Type -> Subsume ()
collectSubstitutions t1 (TNamed _ _ t2) = collectSubstitutions t1 t2
collectSubstitutions (TNamed _ _ t1) t2 = collectSubstitutions t1 t2
collectSubstitutions (TCon _ n1) (TCon _ n2)
  | n1 == n2 = return ()
collectSubstitutions t (TVar (Metadata si) tv) = do
  (Subst s) <- get
  case Map.lookup tv s of
    Just t' | t == t'   -> return ()
            | otherwise -> throwError (SubsumptionError si t t')
    Nothing -> modify (insert tv t)
collectSubstitutions (TFn _ a1 r1) (TFn _ a2 r2) = do
  collectSubstitutions a1 a2
  collectSubstitutions r1 r2
collectSubstitutions (TNoArgFn _ r1) (TNoArgFn _ r2) = collectSubstitutions r1 r2
collectSubstitutions (TForeignFn _ _ a1 r1) (TForeignFn _ _ a2 r2) =
  mapM_ (uncurry collectSubstitutions) (zip a1 a2 ++ zip r1 r2)
collectSubstitutions (TSlice _ t1) (TSlice _ t2) = collectSubstitutions t1 t2
collectSubstitutions (TTuple _ f1 s1 r1) (TTuple _ f2 s2 r2) = do
  collectSubstitutions f1 f2
  collectSubstitutions s1 s2
  zipWithM_ collectSubstitutions r1 r2
collectSubstitutions (TRecord _ r1) (TRecord _ r2) =
  collectSubstitutions r1 r2
collectSubstitutions REmpty{} REmpty{} = return ()
collectSubstitutions r1 r2 | kindOf r1 == Row && kindOf r2 == Row = do
  let f1 = Map.fromList (rowToList r1)
      f2 = Map.fromList (rowToList r2)
      onlyIn1 = f1 `Map.difference` f2
      onlyIn2 = f2 `Map.difference` f1

  unless (Map.null onlyIn1) $
    throwError (SubsumptionError (getSourceInfo r2) r1 r2)

  unless (Map.null onlyIn2) $
    throwError (SubsumptionError (getSourceInfo r2) r1 r2)

  unless (getLeafRow r1 == getLeafRow r2) $
    throwError (SubsumptionError (getSourceInfo r2) r1 r2)

  sequence_ (Map.elems (Map.intersectionWith collectSubstitutions f1 f2))

collectSubstitutions t1 t2 = throwError (SubsumptionError (getSourceInfo t2) t1 t2)


-- | Test if a specific type is subsumed by a more general type. If so, return
-- the corresponding substitution.
typeSubsumedBy :: Type -> Type -> Either SubsumptionError Subst
typeSubsumedBy specific general =
  snd <$> runExcept (runStateT (collectSubstitutions specific general) (Subst Map.empty))

-- | Test if a type scheme is subsumed by an expression with a more general
-- type. If so, return the expression specialized to the less general type (all
-- subexpression types being substituted as well).
subsumedBy :: Scheme -> Typed.TypedExpr -> Either SubsumptionError Typed.CanonicalExpr
subsumedBy s@(Forall _ _ _ st) expr = do
  subst <- typeSubsumedBy st (typeOf expr)
  return (s, apply subst expr)
