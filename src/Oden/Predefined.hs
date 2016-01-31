module Oden.Predefined (
  predefined
) where

import Oden.Identifier
import Oden.Type.Polymorphic
import Oden.Scope

pairs :: [(Identifier, Scheme)]
pairs = [
  (Unqualified "unit", Forall [] typeUnit),

  (Unqualified "not", Forall [] (TFn typeBool typeBool)),

  (Unqualified "len", Forall [TV "a"] (TUncurriedFn [TSlice (TVar (TV "a"))] typeInt))
  ]

predefined :: Scope
predefined = fromList (map toScopeAssoc pairs)
  where
  toScopeAssoc (i, s) = (Predefined, i, ForeignDefinition i s)
