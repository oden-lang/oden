module Oden.Predefined (
  predefined
) where

import Oden.Identifier
import Oden.Type.Polymorphic
import Oden.Scope

pairs :: [(Identifier, Scheme)]
pairs = [
  (Unqualified "unit", Forall [] typeUnit),

  (Unqualified "+", Forall [] (TArr typeInt (TArr typeInt typeInt))),
  (Unqualified "-", Forall [] (TArr typeInt (TArr typeInt typeInt))),
  (Unqualified "*", Forall [] (TArr typeInt (TArr typeInt typeInt))),
  (Unqualified "/", Forall [] (TArr typeInt (TArr typeInt typeInt))),
  (Unqualified "<", Forall [] (TArr typeInt (TArr typeInt typeBool))),
  (Unqualified ">", Forall [] (TArr typeInt (TArr typeInt typeBool))),
  (Unqualified "<=", Forall [] (TArr typeInt (TArr typeInt typeBool))),
  (Unqualified ">=", Forall [] (TArr typeInt (TArr typeInt typeBool))),

  (Unqualified "and", Forall [] (TArr typeBool (TArr typeBool typeBool))),
  (Unqualified "or", Forall [] (TArr typeBool (TArr typeBool typeBool))),
  (Unqualified "not", Forall [] (TArr typeBool typeBool)),

  (Unqualified "==", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) typeBool))),

  (Unqualified "++", Forall [] (TArr typeString (TArr typeString typeString))),

  (Unqualified "len", Forall [TV "a"] (TGoFunc [TSlice (TVar (TV "a"))] typeInt))
  ]

predefined :: Scope
predefined = fromList (map toScopeAssoc pairs)
  where
  toScopeAssoc (i, s) = (Predefined, i, ForeignDefinition i s)
