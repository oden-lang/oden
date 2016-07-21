module Oden.Infer.InferDefinitionSpec where

import           Test.Hspec

import qualified Data.Set as Set

import           Oden.Core.Typed       as Typed
import           Oden.Core.Untyped     as Untyped
import           Oden.Core.Expr
import           Oden.QualifiedName
import           Oden.Environment
import           Oden.Identifier
import qualified Oden.Infer            as Infer
import           Oden.Infer.Environment
import           Oden.Predefined
import           Oden.Type.Polymorphic

import           Oden.Assertions
import           Oden.Infer.Fixtures

inferDefinition :: TypingEnvironment -> Untyped.Definition -> Either Infer.TypeError Typed.TypedDefinition
inferDefinition env def = snd <$> Infer.inferDefinition env def

spec :: Spec
spec = describe "inferDefinition" $ do
  it "infers 'n = 1 + 1'" $
    inferDefinition predef (Untyped.Definition
                            missing
                            (nameInUniverse "n")
                            Nothing
                            (Application
                             missing
                             (Application
                              missing
                              (MethodReference missing (NamedMethodReference (Identifier "Num") (Identifier "Add")) Untyped)
                              (Literal missing (Int 1) Untyped)
                              Untyped)
                             (Literal missing (Int 1) Untyped)
                             Untyped))
    `shouldSucceedWith`
    let constraint = ProtocolConstraint missing (nameInUniverse "Num") typeInt in
    tDefinition
    (nameInUniverse "n")
    (scheme typeInt,
     tApplication
     (tApplication
      (MethodReference
       missing
       (Unresolved (nameInUniverse "Num") (Identifier "Add") constraint)
       (TConstrained (Set.singleton constraint) (typeFn typeInt (typeFn typeInt typeInt))))
      (tLiteral (tInt 1) typeInt)
      (typeFn typeInt typeInt))
     (tLiteral (tInt 1) typeInt)
     typeInt)

  it "infers definition without type signature" $
    inferDefinition empty (Untyped.Definition missing (nameInUniverse "x") Nothing (Literal missing (Int 1) Untyped))
    `shouldSucceedWith`
    tDefinition (nameInUniverse "x") (scheme typeInt, tLiteral (tInt 1) typeInt)

  it "infers polymorphic definition without type signature" $
    shouldSucceed $
      inferDefinition
        empty
        (Untyped.Definition
         missing
         (nameInUniverse "id")
         Nothing
         (Fn missing (NameBinding missing (Identifier "x")) (Symbol missing (Identifier "x") Untyped) Untyped))

  it "infers definition with type signature" $
    inferDefinition predef (Untyped.Definition
                            missing
                            (nameInUniverse "x")
                            (Just $ implicit (tsSymbol (Identifier "int")))
                            (Literal missing (Int 1) Untyped))
    `shouldSucceedWith`
    tDefinition (nameInUniverse "x") (scheme typeInt, tLiteral (tInt 1) typeInt)

  it "infers polymorphic definition with type signature" $
    inferDefinition empty (Untyped.Definition
                           missing
                           (nameInUniverse "id")
                           (Just $ explicit [varBinding "a"] (tsFn (tsVar "a") (tsVar "a")))
                           (Fn missing (NameBinding missing (Identifier "x")) (Symbol missing (Identifier "x") Untyped) Untyped))
    `shouldSucceedWith`
    tDefinition (nameInUniverse "id") ( scheme (typeFn tvarA tvarA)
                                      , tFn
                                        (tNameBinding (Identifier "x"))
                                        (tSymbol (Identifier "x") tvarA)
                                        (typeFn tvarA tvarA))

  it "fails when specified type signature does not unify" $
    shouldFail $
      inferDefinition empty (Untyped.Definition
                             missing
                             (nameInUniverse "some-number")
                             (Just $ implicit (tsSymbol (Identifier "bool")))
                             (Literal missing (Int 1) Untyped))

  it "infers twice function with correct type signature" $
    inferDefinition
    empty
    (Untyped.Definition
     missing
     (nameInUniverse "twice")
     (Just $ explicit [varBinding "a"] (tsFn (tsFn (tsVar "a") (tsVar "a")) (tsFn (tsVar "a") (tsVar "a"))))
     twiceUntyped)
    `shouldSucceedWith`
    twiceTyped

  it "fails on twice function with incorrect type signature" $
    shouldFail $
      inferDefinition empty (Untyped.Definition
                             missing
                             (nameInUniverse "twice")
                             (Just $ explicit [varBinding "a"] (tsFn (tsVar "a") (tsVar "a")))
                             twiceUntyped)

  it "infers recursive definition" $
    inferDefinition
    predef
    (Untyped.Definition
     missing
     (nameInUniverse "f")
     (Just $ implicit (tsFn (tsSymbol (Identifier "int")) (tsSymbol (Identifier "int"))))
     countToZero)
    `shouldSucceedWith`
    countToZeroTyped

  it "infers recursive definition without type signature" $
    inferDefinition predef (Untyped.Definition missing (nameInUniverse "f") Nothing countToZero)
    `shouldSucceedWith`
    countToZeroTyped

  it "fails on recursive with incorrect signature" $
    shouldFail $
      inferDefinition
        predef
        (Untyped.Definition
         missing
         (nameInUniverse "f")
         (Just $ implicit (tsFn (tsSymbol (Identifier "int")) (tsSymbol (Identifier "any"))))
         countToZero)

  it "infers record field access fn definition with type signature" $
    let recordType = typeRecord (rowExt (Identifier "foo") tvarA tvarB)
        functionType = typeFn recordType tvarA in
    inferDefinition
    predef
    (Untyped.Definition
     missing
     (nameInUniverse "f")
     (Just $ explicit [varBinding "a", varBinding "b"] (tsFn (tsRecord (tsRowExt (Identifier "foo") (tsVar "a") (tsVar "b"))) (tsVar "a")))
     (Fn
      missing
      (NameBinding missing (Identifier "x"))
      (MemberAccess
       missing
       (NamedMemberAccess (Symbol missing (Identifier "x") Untyped) (Identifier "foo"))
       Untyped)
      Untyped))
    `shouldSucceedWith`
    tDefinition
    (nameInUniverse "f")
    (scheme functionType,
     tFn
     (tNameBinding (Identifier "x"))
     (MemberAccess
      missing
      (RecordFieldAccess (tSymbol (Identifier "x") recordType) (Identifier "foo"))
      tvarA)
     functionType)
