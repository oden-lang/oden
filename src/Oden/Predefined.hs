module Oden.Predefined (
  predefined
) where

import Oden.Identifier
import Oden.Type.Polymorphic
import Oden.Scope

pairs :: [(Identifier, Scheme)]
pairs = [
  (Unqualified "unit", Forall [] typeUnit),

  (Unqualified "+", Forall [] (TUncurriedFn [typeInt, typeInt] typeInt)),
  (Unqualified "-", Forall [] (TFn typeInt (TFn typeInt typeInt))),
  (Unqualified "*", Forall [] (TFn typeInt (TFn typeInt typeInt))),
  (Unqualified "/", Forall [] (TFn typeInt (TFn typeInt typeInt))),
  (Unqualified "<", Forall [] (TFn typeInt (TFn typeInt typeBool))),
  (Unqualified ">", Forall [] (TFn typeInt (TFn typeInt typeBool))),
  (Unqualified "<=", Forall [] (TFn typeInt (TFn typeInt typeBool))),
  (Unqualified ">=", Forall [] (TFn typeInt (TFn typeInt typeBool))),

  (Unqualified "and", Forall [] (TFn typeBool (TFn typeBool typeBool))),
  (Unqualified "or", Forall [] (TFn typeBool (TFn typeBool typeBool))),
  (Unqualified "not", Forall [] (TFn typeBool typeBool)),

  (Unqualified "==", Forall [TV "a"] (TFn (TVar (TV "a")) (TFn (TVar (TV "a")) typeBool))),

  (Unqualified "++", Forall [] (TFn typeString (TFn typeString typeString))),

  (Unqualified "len", Forall [TV "a"] (TUncurriedFn [TSlice (TVar (TV "a"))] typeInt))
  ]

predefined :: Scope
predefined = fromList (map toScopeAssoc pairs)
  where
  toScopeAssoc (i, s) = (Predefined, i, ForeignDefinition i s)
