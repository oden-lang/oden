module Oden.Predefined (
  universe
) where

import Oden.Identifier
import Oden.Metadata
import Oden.Type.Basic
import Oden.Type.Polymorphic
import Oden.SourceInfo
import qualified Oden.Core as Core

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

pairs :: [(Identifier, Scheme)]
pairs = [
  (Identifier "len", Forall predefined [TVarBinding predefined (TV "a")] (TUncurriedFn predefined [TSlice predefined (TVar predefined (TV "a"))] (TBasic predefined TInt)))
  ]

universe :: Core.Package
universe =
  Core.Package
  (Core.PackageDeclaration (Metadata Missing) [])
  []
  (map toDefinition pairs)
    where
    toDefinition (i, s) = Core.ForeignDefinition predefined i s
