module Oden.Predefined (
  universe,
  typeInt,
  typeString,
  typeBool,
  typeUnit
) where

import Oden.Identifier
import Oden.Metadata
import Oden.QualifiedName
import Oden.Type.Polymorphic
import Oden.SourceInfo
import qualified Oden.Core as Core

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

typeInt, typeString, typeBool, typeUnit :: Type
typeInt = TCon predefined (nameInUniverse "int")
typeString = TCon predefined (nameInUniverse "string")
typeBool = TCon predefined (nameInUniverse "bool")
typeUnit = TCon predefined (nameInUniverse "unit")

foreignFns :: [(Identifier, Scheme)]
foreignFns = [
  (Identifier "len", Forall predefined [TVarBinding predefined (TV "a")] (TForeignFn predefined False [TSlice predefined (TVar predefined (TV "a"))] [typeInt])),
  (Identifier "print", Forall predefined [] (TForeignFn predefined False [TAny predefined] [typeUnit])),
  (Identifier "println", Forall predefined [] (TForeignFn predefined False [TAny predefined] [typeUnit]))
  ]

types :: [(String, Type)]
types = [
  ("int", typeInt),
  ("string", typeString),
  ("bool", typeBool),
  ("unit", typeUnit)
  ]

universe :: Core.Package
universe =
  Core.Package
  (Core.PackageDeclaration (Metadata Missing) [])
  []
  (map toForeignDef foreignFns ++ map toTypeDef types)
    where
    toTypeDef (s, t) = Core.TypeDefinition predefined (nameInUniverse s) [] t
    toForeignDef (i, s) = Core.ForeignDefinition predefined i s
