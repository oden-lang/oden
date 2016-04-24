module Oden.Predefined (
  universe,
  typeInt,
  typeString,
  typeBool,
  typeUnit
) where

import qualified Oden.Core             as Core
import           Oden.Core.Package
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Data.Set              hiding (map)

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

typeInt, typeString, typeBool, typeUnit :: Type
typeInt = TCon predefined (nameInUniverse "int")
typeString = TCon predefined (nameInUniverse "string")
typeBool = TCon predefined (nameInUniverse "bool")
typeUnit = TCon predefined (nameInUniverse "unit")

foreignFns :: [(Identifier, Scheme)]
foreignFns = [
  (Identifier "len", Forall predefined [TVarBinding predefined (TV "a")] empty (TForeignFn predefined False [TSlice predefined (TVar predefined (TV "a"))] [typeInt])),
  (Identifier "print", Forall predefined [TVarBinding predefined (TV "a")] empty (TForeignFn predefined False [TVar predefined (TV "a")] [typeUnit])),
  (Identifier "println", Forall predefined [TVarBinding predefined (TV "a")] empty (TForeignFn predefined False [TVar predefined (TV "a")] [typeUnit]))
  ]

types :: [(String, Type)]
types = [
  ("int", typeInt),
  ("string", typeString),
  ("bool", typeBool),
  ("unit", typeUnit)
  ]

universe :: Core.TypedPackage
universe =
  Package
  (PackageDeclaration (Metadata Missing) [])
  []
  (map toForeignDef foreignFns ++ map toTypeDef types)
    where
    toTypeDef (s, t) = Core.TypeDefinition predefined (nameInUniverse s) [] t
    toForeignDef (i, s) = Core.ForeignDefinition predefined i s
