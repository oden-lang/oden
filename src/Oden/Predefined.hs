module Oden.Predefined (
  universe
) where

import Oden.Identifier
import Oden.Type.Basic
import Oden.Type.Polymorphic
import Oden.SourceInfo
import qualified Oden.Core as Core

pairs :: [(Identifier, Scheme)]
pairs = [
  (Identifier "len", Forall Predefined [TVarBinding Predefined (TV "a")] (TUncurriedFn Predefined [TSlice Predefined (TVar Predefined (TV "a"))] (TBasic Predefined TInt)))
  ]

universe :: Core.Package
universe =
  Core.Package
  (Core.PackageDeclaration Missing [])
  []
  (map toDefinition pairs)
    where
    toDefinition (i, s) = Core.ForeignDefinition Predefined i s
