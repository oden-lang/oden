module Oden.Predefined (
  predefined
) where

import Oden.Identifier
import Oden.Type.Basic
import Oden.Type.Polymorphic
import qualified Oden.SourceInfo as SI
import Oden.Scope

pairs :: [(Identifier, Scheme)]
pairs = [
  (Unqualified "unit", Forall SI.Predefined [] (TUnit SI.Predefined)),

  (Unqualified "not", Forall SI.Predefined [] (TFn SI.Predefined (TBasic SI.Predefined TBool) (TBasic SI.Predefined TBool))),

  (Unqualified "len", Forall SI.Predefined [TVarBinding SI.Predefined (TV "a")] (TUncurriedFn SI.Predefined [TSlice SI.Predefined (TVar SI.Predefined (TV "a"))] (TBasic SI.Predefined TInt)))
  ]

predefined :: Scope
predefined = fromList (map toScopeAssoc pairs)
  where
  toScopeAssoc (i, s) = (Predefined, i, ForeignDefinition i s)
