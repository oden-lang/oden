{-# LANGUAGE LambdaCase #-}
module Oden.Compiler.ResolutionSpec where

import           Oden.Compiler.Environment
import           Oden.Compiler.Resolution

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Typed

import           Oden.Infer.ConstraintCollection

import qualified Oden.Environment                 as Environment
import           Oden.Identifier
import           Oden.Metadata
import           Oden.Predefined
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import           Test.Hspec

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

tvA = TV "a"

tvarA = TVar predefined tvA
symbol s = Symbol missing (Identifier s)

boolToBool = TFn predefined typeBool typeBool
aToBool = TFn predefined tvarA typeBool

one = Literal missing (Int 1) typeInt

rowWithResultField =
  rowFromList [(Identifier "result", typeBool)] (REmpty missing)

rowWithResultAndMessageField =
  rowFromList [ (Identifier "result", typeBool)
              , (Identifier "message", typeString)] (REmpty missing)

unresolved protocol method type' constraintType =
  let constraint = ProtocolConstraint missing protocol constraintType
  in MethodReference
     missing
     (Unresolved protocol method constraint)
     (TConstrained
      (Set.singleton constraint)
      type')

resolved protocol method impl =
  MethodReference missing (Resolved protocol method impl)

testableProtocolName = nameInUniverse "Testable"
testableMethodName = Identifier "test"

testableProtocol =
  Protocol
  predefined
  testableProtocolName
  (TVar predefined tvA)
  [testableProtocolMethod]

testableProtocolMethod =
  ProtocolMethod
  predefined
  testableMethodName
  (Forall predefined [] Set.empty (TFn predefined tvarA typeBool))

testableMethodImplementation :: String -> Type -> MethodImplementation TypedExpr
testableMethodImplementation identifier t =
  MethodImplementation
  missing
  testableMethodName
  (symbol identifier (TFn missing t typeBool))

testableImplementation :: String -> Type -> ProtocolImplementation TypedExpr
testableImplementation identifier t =
  ProtocolImplementation
  missing
  testableProtocolName
  t
  [testableMethodImplementation identifier t]

implementationsAsSet :: Either ResolutionError a
                     -> Set (ProtocolImplementation TypedExpr)
implementationsAsSet = \case
  Left (MultipleMatchingImplementationsInScope _ impls) -> Set.fromList impls
  _ -> Set.empty

testPkg = PackageDeclaration missing (NativePackageName ["test"])

exprAsDefinition :: String
                 -> TypedExpr
                 -> Definition TypedExpr
exprAsDefinition name expr =
  Definition
   missing
   (Identifier name)
   (Forall missing quantifiers constraints (typeOf expr), expr)
  where
  quantifiers = map (TVarBinding missing) (Set.toList $ ftv expr)
  constraints = collectConstraints expr

packageWithExpr :: String
                -> TypedExpr
                -> TypedPackage
packageWithExpr name expr =
  TypedPackage
  testPkg
  []
  [exprAsDefinition name expr]

spec :: Spec
spec =
  describe "resolveInPackage" $ do

    it "throws error if there's no matching implementation" $
      shouldFail
      (resolveInPackage
       Environment.empty
       (packageWithExpr
        "foo"
        (unresolved
         testableProtocolName
         testableMethodName
         boolToBool
         typeBool)))

    it "does not try to resolve constraints in unused polymorphic definitions" $
      let expr = unresolved
                 testableProtocolName
                 testableMethodName
                 aToBool
                 tvarA
          pkg = packageWithExpr "foo" expr in
      resolveInPackage Environment.empty pkg
      `shouldSucceedWith`
      TypedPackage testPkg [] []

    it "resolves a single matching implementation" $
      resolveInPackage
      (Environment.singletonImplementation (testableImplementation "foo" typeBool))
      (packageWithExpr
       "foo"
       (unresolved
        testableProtocolName
        testableMethodName
        boolToBool
        typeBool))
      `shouldSucceedWith`
      packageWithExpr
      "foo"
      (resolved
       testableProtocolName
       testableMethodName
       (testableMethodImplementation "foo" typeBool)
       boolToBool)

    it "resolves a single matching implementation for a record type" $
      resolveInPackage
      (Environment.singletonImplementation (testableImplementation "foo" rowWithResultField))
      (packageWithExpr
       "foo"
       (Application
        missing
        (unresolved
         testableProtocolName
         testableMethodName
         (TFn missing rowWithResultField typeBool)
         rowWithResultField)
        (symbol "recordValue" rowWithResultField)
       typeBool))
      `shouldSucceedWith`
      packageWithExpr
      "foo"
      (Application
       missing
       (resolved
        testableProtocolName
        testableMethodName
        (testableMethodImplementation "foo" rowWithResultField)
        (TFn missing rowWithResultField typeBool))
       (symbol "recordValue" rowWithResultField)
       typeBool)

    it "resolves the correct matching implementation for a record type" $
      resolveInPackage
      (Environment.fromLists [] [ testableImplementation "foo" rowWithResultField
                                , testableImplementation "bar" rowWithResultAndMessageField ])
      (packageWithExpr
       "foo"
       (Application
        missing
        (unresolved
         testableProtocolName
         testableMethodName
         (TFn missing rowWithResultField typeBool)
         rowWithResultField)
        (symbol "recordValue" rowWithResultField)
        typeBool))
      `shouldSucceedWith`
      packageWithExpr
      "foo"
      (Application
        missing
       (resolved
        testableProtocolName
        testableMethodName
        (testableMethodImplementation "foo" rowWithResultField)
        (TFn missing rowWithResultField typeBool))
       (symbol "recordValue" rowWithResultField)
       typeBool)

    it "throws error if there's multiple matching implementations" $
      implementationsAsSet
      (resolveInPackage
       (Environment.fromLists [] [ testableImplementation "foo" typeBool
                                 , testableImplementation "bar" typeBool
                                 ])
       (packageWithExpr
        "foo"
        (unresolved
         testableProtocolName
         testableMethodName
         boolToBool
         typeBool)))
      `shouldBe`
      Set.fromList [ testableImplementation "bar" typeBool
                   , testableImplementation "foo" typeBool
                   ]

    it "instantiates referenced polymorphic definitions with constraints" $
      let alias = exprAsDefinition
                  "alias"
                  (unresolved
                   testableProtocolName
                   testableMethodName
                   (TFn missing tvarA typeBool)
                   tvarA)
          constraint = ProtocolConstraint missing testableProtocolName typeInt
          referencing = exprAsDefinition
                        "referencing"
                        (Application
                         missing
                         (Symbol missing (Identifier "alias") (TConstrained
                                                               (Set.singleton constraint)
                                                               (TFn missing typeInt typeBool)))
                         (Literal missing (Int 0) typeInt)
                         typeBool)
      in
      resolveInPackage
      (Environment.fromLists
       [ (Identifier "alias", DefinitionBinding alias)
       , (Identifier "referencing", DefinitionBinding referencing)
       ]
       [testableImplementation "foo" typeInt])
      (TypedPackage
       testPkg
       []
       [alias, referencing])
      `shouldSucceedWith`
      TypedPackage
      testPkg
      []
      [ exprAsDefinition
        "alias_inst_int_to_bool"
        (resolved
         testableProtocolName
         testableMethodName
         (testableMethodImplementation "foo" typeInt)
         (TFn missing typeInt typeBool))
      , exprAsDefinition
        "referencing"
        (Application
         missing
         (Symbol missing (Identifier "alias_inst_int_to_bool") (TFn missing typeInt typeBool))
         (Literal missing (Int 0) typeInt)
         typeBool)
      ]
