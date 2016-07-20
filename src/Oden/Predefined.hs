module Oden.Predefined (
  universe,
  typeInt,
  typeFloat64,
  typeString,
  typeBool,
  typeUnit
) where

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Foreign
import           Oden.Core.Package
import           Oden.Core.Typed
import           Oden.Core.ProtocolImplementation

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo

import           Oden.Type.Polymorphic

import           Data.Set              hiding (map)

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

tvarA :: Type
tvarA = TVar predefined (TV "a")

typeInt, typeFloat64, typeString, typeBool, typeUnit :: Type
typeInt = TCon predefined (nameInUniverse "int")
typeFloat64 = TCon predefined (nameInUniverse "float64")
typeString = TCon predefined (nameInUniverse "string")
typeBool = TCon predefined (nameInUniverse "bool")
typeUnit = TCon predefined (nameInUniverse "unit")

errorProtocol :: Protocol
errorProtocol =
  Protocol
  predefined
  (nameInUniverse "error")
  (TVar predefined (TV "a"))
  [ProtocolMethod
   predefined
   (Identifier "Error")
   (Forall predefined [] empty (TFn predefined (TVar predefined (TV "a")) typeString))]

equalityProtocol :: Protocol
equalityProtocol =
  Protocol
  predefined
  (nameInUniverse "Equality")
  (TVar predefined (TV "a"))
  [ProtocolMethod
   predefined
   (Identifier "EqualTo")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA typeBool)))
  ,ProtocolMethod
   predefined
   (Identifier "NotEqualTo")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA typeBool)))]

orderedProtocol :: Protocol
orderedProtocol =
  Protocol
  predefined
  (nameInUniverse "Ordered")
  (TVar predefined (TV "a"))
  [ProtocolMethod
   predefined
   (Identifier "LessThan")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA typeBool)))
  ,ProtocolMethod
   predefined
   (Identifier "LessThanEqual")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA typeBool)))
  ,ProtocolMethod
   predefined
   (Identifier "GreaterThan")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA typeBool)))
  ,ProtocolMethod
   predefined
   (Identifier "GreaterThanEqual")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA typeBool)))]

logicalProtocol :: Protocol
logicalProtocol =
  Protocol
  predefined
  (nameInUniverse "Logical")
  (TVar predefined (TV "a"))
  [ProtocolMethod
   predefined
   (Identifier "Conjunction")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA tvarA)))
  ,ProtocolMethod
   predefined
   (Identifier "Disjunction")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA tvarA)))
  ,ProtocolMethod
   predefined
   (Identifier "Not")
   (Forall predefined [] empty (TFn predefined tvarA tvarA))]

monoidProtocol :: Protocol
monoidProtocol =
  Protocol
  predefined
  (nameInUniverse "Monoid")
  (TVar predefined (TV "a"))
  [ProtocolMethod
   predefined
   (Identifier "Apply")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA tvarA)))
  ,ProtocolMethod
   predefined
   (Identifier "Identity")
   (Forall predefined [] empty tvarA)
  ]

numProtocol :: Protocol
numProtocol =
  Protocol
  predefined
  (nameInUniverse "Num")
  (TVar predefined (TV "a"))
  [ProtocolMethod
   predefined
   (Identifier "Add")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA tvarA)))
  ,ProtocolMethod
   predefined
   (Identifier "Subtract")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA tvarA)))
  ,ProtocolMethod
   predefined
   (Identifier "Multiply")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA tvarA)))
  ,ProtocolMethod
   predefined
   (Identifier "Divide")
   (Forall predefined [] empty (TFn predefined tvarA (TFn predefined tvarA tvarA)))
  ,ProtocolMethod
   predefined
   (Identifier "Negate")
   (Forall predefined [] empty (TFn predefined tvarA tvarA))
  ]


protocols :: [(String, Protocol)]
protocols =
  [("error", errorProtocol)
  ,("Num", numProtocol)
  ,("Equality", equalityProtocol)
  ,("Ordered", orderedProtocol)
  ,("Logical", logicalProtocol)
  ,("Monoid", monoidProtocol)
  ]

impls :: [ProtocolImplementation TypedExpr]
impls =
  -- Equality(int)
  [ProtocolImplementation
   predefined
   (nameInUniverse "Equality")
   typeInt
   [MethodImplementation
    predefined
    (Identifier "EqualTo")
    (Foreign predefined
     (ForeignBinaryOperator Equals)
     (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "NotEqualTo")
     (Foreign predefined
      (ForeignBinaryOperator Equals)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))]

  -- Equality(float64)
  ,ProtocolImplementation
   predefined
   (nameInUniverse "Equality")
   typeFloat64
   [MethodImplementation
    predefined
    (Identifier "EqualTo")
    (Foreign predefined
     (ForeignBinaryOperator Equals)
     (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "NotEqualTo")
     (Foreign predefined
      (ForeignBinaryOperator Equals)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))]

  -- Ordered(int)
  ,ProtocolImplementation
   predefined
   (nameInUniverse "Ordered")
   typeInt
   [MethodImplementation
    predefined
    (Identifier "LessThan")
    (Foreign predefined
     (ForeignBinaryOperator LessThan)
     (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "LessThanEqual")
     (Foreign predefined
      (ForeignBinaryOperator LessThanEqual)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "GreaterThan")
     (Foreign predefined
      (ForeignBinaryOperator GreaterThan)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "GreaterThanEqual")
     (Foreign predefined
      (ForeignBinaryOperator GreaterThanEqual)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))]

  -- Ordered(float64)
  ,ProtocolImplementation
   predefined
   (nameInUniverse "Ordered")
   typeFloat64
   [MethodImplementation
    predefined
    (Identifier "LessThan")
    (Foreign predefined
     (ForeignBinaryOperator LessThan)
     (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "LessThanEqual")
     (Foreign predefined
      (ForeignBinaryOperator LessThanEqual)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "GreaterThan")
     (Foreign predefined
      (ForeignBinaryOperator GreaterThan)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))
    ,MethodImplementation
     predefined
     (Identifier "GreaterThanEqual")
     (Foreign predefined
      (ForeignBinaryOperator GreaterThanEqual)
      (TFn predefined typeInt (TFn predefined typeInt typeBool)))]

   -- Num(int)
  ,ProtocolImplementation
   predefined
   (nameInUniverse "Num")
   typeInt
   [MethodImplementation
    predefined
    (Identifier "Add")
    (Foreign predefined (ForeignBinaryOperator Add) (TFn predefined typeInt (TFn predefined typeInt typeInt)))
   ,MethodImplementation
    predefined
    (Identifier "Subtract")
    (Foreign predefined (ForeignBinaryOperator Subtract) (TFn predefined typeInt (TFn predefined typeInt typeInt)))
   ,MethodImplementation
    predefined
    (Identifier "Multiply")
    (Foreign predefined (ForeignBinaryOperator Multiply) (TFn predefined typeInt (TFn predefined typeInt typeInt)))
   ,MethodImplementation
    predefined
    (Identifier "Divide")
    (Foreign predefined (ForeignBinaryOperator Divide) (TFn predefined typeInt (TFn predefined typeInt typeInt)))
   ,MethodImplementation
    predefined
    (Identifier "Negate")
    (Foreign predefined (ForeignUnaryOperator Negate) (TFn predefined typeInt (TFn predefined typeInt typeInt)))]

   -- Num(float64)
  ,ProtocolImplementation
   predefined
   (nameInUniverse "Num")
   typeFloat64
   [MethodImplementation
    predefined
    (Identifier "Add")
    (Foreign predefined (ForeignBinaryOperator Add) (TFn predefined typeFloat64 (TFn predefined typeFloat64 typeFloat64)))
   ,MethodImplementation
    predefined
    (Identifier "Subtract")
    (Foreign predefined (ForeignBinaryOperator Subtract) (TFn predefined typeFloat64 (TFn predefined typeFloat64 typeFloat64)))
   ,MethodImplementation
    predefined
    (Identifier "Multiply")
    (Foreign predefined (ForeignBinaryOperator Multiply) (TFn predefined typeFloat64 (TFn predefined typeFloat64 typeFloat64)))
   ,MethodImplementation
    predefined
    (Identifier "Divide")
    (Foreign predefined (ForeignBinaryOperator Divide) (TFn predefined typeFloat64 (TFn predefined typeFloat64 typeFloat64)))
   ,MethodImplementation
    predefined
    (Identifier "Negate")
    (Foreign predefined (ForeignUnaryOperator Negate) (TFn predefined typeFloat64 (TFn predefined typeFloat64 typeFloat64)))]

  -- Monoid(string)
  ,ProtocolImplementation
    predefined
    (nameInUniverse "Monoid")
    typeString
    [MethodImplementation
     predefined
     (Identifier "Apply")
     (Foreign predefined (ForeignBinaryOperator Add) (TFn predefined typeString (TFn predefined typeString typeString)))
    ,MethodImplementation
     predefined
     (Identifier "Identity")
     (Literal predefined (String "") typeString)]

  -- Logical(bool)
  ,ProtocolImplementation
   predefined
   (nameInUniverse "Logical")
   typeBool
   [MethodImplementation
    predefined
    (Identifier "Conjunction")
    (Foreign predefined (ForeignBinaryOperator And) (TFn predefined typeBool (TFn predefined typeBool typeBool)))
   ,MethodImplementation
    predefined
    (Identifier "Disjunction")
    (Foreign predefined (ForeignBinaryOperator Or) (TFn predefined typeBool (TFn predefined typeBool typeBool)))
   ,MethodImplementation
    predefined
    (Identifier "Not")
    (Foreign predefined (ForeignUnaryOperator Not) (TFn predefined typeBool typeBool))]
  ]

foreignFns :: [(String, Scheme)]
foreignFns = [
  ("len", Forall predefined [TVarBinding predefined (TV "a")] empty (TForeignFn predefined False [TSlice predefined (TVar predefined (TV "a"))] [typeInt])),
  ("print", Forall predefined [TVarBinding predefined (TV "a")] empty (TForeignFn predefined False [TVar predefined (TV "a")] [typeUnit])),
  ("println", Forall predefined [TVarBinding predefined (TV "a")] empty (TForeignFn predefined False [TVar predefined (TV "a")] [typeUnit]))
  ]

types :: [(String, Type)]
types = [
  ("int", typeInt),
  ("float64", typeFloat64),
  ("string", typeString),
  ("bool", typeBool),
  ("unit", typeUnit)
  ]

universe :: TypedPackage
universe =
  TypedPackage
  (PackageDeclaration (Metadata Predefined) (NativePackageName []))
  []
  (concat [ map toProtocolDef protocols
          , map toForeignDef foreignFns
          , map toTypeDef types
          , map (Implementation predefined) impls
          ])
    where
    toProtocolDef (s, p) = ProtocolDefinition predefined (nameInUniverse s) p
    toTypeDef (s, t) = TypeDefinition predefined (nameInUniverse s) [] t
    toForeignDef (i, s) = ForeignDefinition predefined (nameInUniverse i) s
