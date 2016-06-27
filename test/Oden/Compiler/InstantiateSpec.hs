module Oden.Compiler.InstantiateSpec where

import           Test.Hspec

import           Oden.Compiler.Instantiate

import           Oden.Core.Expr
import           Oden.Core.Typed

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic     as Poly

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

tvA :: Poly.TVar
tvA = Poly.TV "a"

tvB :: Poly.TVar
tvB = Poly.TV "b"

tvarA :: Poly.Type
tvarA = Poly.TVar missing tvA

tvarB :: Poly.Type
tvarB = Poly.TVar missing tvB

typeInt = Poly.TCon missing (nameInUniverse "int")
typeBool = Poly.TCon missing (nameInUniverse "bool")
typeString = Poly.TCon missing (nameInUniverse "string")

identityPoly :: TypedExpr
identityPoly =
  Fn
  missing
  (NameBinding missing (Identifier "x"))
  (Symbol missing (Identifier "x") tvarA)
  (Poly.TFn missing tvarA tvarA)

identityIntType :: Poly.Type
identityIntType = Poly.TFn missing typeInt typeInt

identityInt :: TypedExpr
identityInt =
  Fn
  missing
  (NameBinding missing (Identifier "x"))
  (Symbol missing (Identifier "x") typeInt)
  (Poly.TFn missing typeInt typeInt)

lenType :: Poly.Type
lenType = Poly.TForeignFn missing False [Poly.TSlice missing tvarA] [typeInt]

lenPoly :: TypedExpr
lenPoly = Symbol missing (Identifier "len") lenType

lenInt :: TypedExpr
lenInt =
  Symbol
  missing
  (Identifier "len")
  (Poly.TForeignFn missing False [Poly.TSlice missing typeInt] [typeInt])

pairPoly :: TypedExpr
pairPoly =
  Tuple
  missing
  (Symbol missing (Identifier "x") tvarA)
  (Symbol missing (Identifier "y") (Poly.TVar missing (Poly.TV "b")))
  []
  (Poly.TTuple missing tvarA (Poly.TVar missing (Poly.TV "b")) [])


pairIntStringType :: Poly.Type
pairIntStringType = Poly.TTuple missing typeInt typeString []

pairIntString :: TypedExpr
pairIntString =
  Tuple
  missing
  (Symbol missing (Identifier "x") typeInt)
  (Symbol missing (Identifier "y") typeString)
  []
  (Poly.TTuple missing typeInt typeString [])

recordPoly :: TypedExpr
recordPoly =
  RecordInitializer
  missing
  [FieldInitializer missing (Identifier "foo") (Symbol missing (Identifier "x") tvarA)]
  (Poly.TRecord
   missing
   (Poly.RExtension missing (Identifier "foo") tvarA (Poly.REmpty missing)))

recordIntType :: Poly.Type
recordIntType =
  Poly.TRecord
  missing
  (Poly.RExtension missing (Identifier "foo") typeInt (Poly.REmpty missing))

recordInt :: TypedExpr
recordInt =
  RecordInitializer
  missing
  [FieldInitializer missing (Identifier "foo") (Symbol missing (Identifier "x") typeInt)]
  (Poly.TRecord
   missing
   (Poly.RExtension missing (Identifier "foo") typeInt (Poly.REmpty missing)))

polyRecordIntAndRowVariable :: Poly.Type
polyRecordIntAndRowVariable =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   tvarA)

polyRecordInt :: Poly.Type
polyRecordInt =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   (Poly.REmpty missing))

polyRecordIntAndString :: Poly.Type
polyRecordIntAndString =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   (Poly.RExtension
    missing
    (Identifier "b")
    typeString
    (Poly.REmpty missing)))

fieldAccessPoly :: Poly.Type -> TypedExpr
fieldAccessPoly recordType =
  Fn
  missing
  (NameBinding missing (Identifier "x"))
  (MemberAccess
   missing
   (RecordFieldAccess
    (Symbol missing (Identifier "x") recordType)
    (Identifier "a"))
   typeInt)
  (Poly.TFn
   missing
   recordType
   typeInt)

fieldAccessOnlyIntType :: Poly.Type
fieldAccessOnlyIntType =
  Poly.TFn
  missing
  (Poly.TRecord missing (Poly.RExtension missing (Identifier "a") typeInt (Poly.REmpty missing)))
  typeInt

typeRecordEmpty :: Poly.Type
typeRecordEmpty = Poly.TRecord missing (Poly.REmpty missing)

typeRecordInt :: Poly.Type
typeRecordInt =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   (Poly.REmpty missing))

typeRecordIntAndString :: Poly.Type
typeRecordIntAndString =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   (Poly.RExtension
    missing
    (Identifier "b")
    typeString
    (Poly.REmpty missing)))

typeFieldAccess r = Poly.TFn missing r typeInt

fieldAccessWrongLabelType :: Poly.Type
fieldAccessWrongLabelType =
  Poly.TFn
  missing
  (Poly.TRecord missing (Poly.RExtension missing (Identifier "b") typeInt (Poly.REmpty missing)))
  typeInt

spec :: Spec
spec =
  describe "instantiate" $ do
    it "does not change typemorphic function" $
      instantiate identityInt identityIntType `shouldSucceedWith` identityInt
    it "instantiates identity function with int" $
      instantiate identityPoly identityIntType `shouldSucceedWith` identityInt
    it "instantiates len func" $
      instantiate lenPoly (typeOf lenInt) `shouldSucceedWith` lenInt
    it "instantiates polymorphic tuple" $
      instantiate pairPoly pairIntStringType `shouldSucceedWith` pairIntString
    it "instantiates record initializer with int field" $
      instantiate recordPoly recordIntType `shouldSucceedWith` recordInt
    it "instantiates record field access with only an int field" $
      instantiate
      (fieldAccessPoly polyRecordIntAndRowVariable)
      (typeFieldAccess typeRecordInt)
      `shouldSucceedWith`
      fieldAccessPoly polyRecordInt
    it "instantiates record field access with both int and string field" $
      instantiate
      (fieldAccessPoly polyRecordIntAndRowVariable)
      (typeFieldAccess typeRecordIntAndString)
      `shouldSucceedWith`
      fieldAccessPoly polyRecordIntAndString
    it "fails on mismatching row labels" $
      shouldFail $
        instantiate (fieldAccessPoly polyRecordIntAndRowVariable) fieldAccessWrongLabelType
