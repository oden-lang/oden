module Oden.Infer.InferDefinitionSpec where

import           Test.Hspec

import qualified Oden.Core             as Core
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Core.Operator
import           Oden.Environment
import           Oden.Identifier
import qualified Oden.Infer            as Infer
import           Oden.Infer            (inferExpr)
import           Oden.Infer.Environment
import           Oden.Predefined
import           Oden.Type.Polymorphic

import           Oden.Assertions
import           Oden.Infer.Fixtures

inferDefinition :: TypingEnvironment -> Untyped.Definition -> Either Infer.TypeError Core.Definition
inferDefinition env def = snd <$> Infer.inferDefinition env def

spec :: Spec
spec = describe "inferDefinition" $ do
  it "infers 'n = 1 + 1'" $
    inferDefinition predef (uDefinition (Identifier "n") Nothing (uOp
                                                            Add
                                                            (uLiteral (uInt 1))
                                                            (uLiteral (uInt 1))))
    `shouldSucceedWith`
    tDefinition
    (Identifier "n")
    (forall [] typeInt,
     tOp
     Add
     (tLiteral (tInt 1) typeInt)
     (tLiteral (tInt 1) typeInt)
     typeInt)

  it "infers definition without type signature" $
    inferDefinition empty (uDefinition (Identifier "x") Nothing (uLiteral (uInt 1)))
    `shouldSucceedWith`
    tDefinition (Identifier "x") (forall [] typeInt, tLiteral (tInt 1) typeInt)

  it "infers polymorphic definition without type signature" $
    shouldSucceed $
      inferDefinition
        empty
        (uDefinition (Identifier "id")
                            Nothing
                            (uFn (uNameBinding (Identifier "x")) (uSymbol (Identifier "x"))))

  it "infers definition with type signature" $
    inferDefinition empty (uDefinition (Identifier "x") (Just $ implicit (tsSymbol (Identifier "any"))) (uLiteral (uInt 1)))
    `shouldSucceedWith`
    tDefinition (Identifier "x") (forall [] typeAny, tLiteral (tInt 1) typeInt)

  it "infers polymorphic definition with type signature" $
    inferDefinition empty (uDefinition (Identifier "id")
                                              (Just $ explicit [varBinding "a"] (tsFn (tsVar "a") (tsVar "a")))
                                              (uFn (uNameBinding (Identifier "x")) (uSymbol (Identifier "x"))))
    `shouldSucceedWith`
    tDefinition (Identifier "id") (forall [tvarBinding tvA] (typeFn tvarA tvarA),
                          tFn (tNameBinding (Identifier "x")) (tSymbol (Identifier "x") tvarA) (typeFn tvarA tvarA))

  it "fails when specified type signature does not unify" $
    shouldFail $
      inferDefinition empty (uDefinition (Identifier "some-number")
                                                (Just $ implicit (tsSymbol (Identifier "bool")))
                                                (uLiteral (uInt 1)))

  it "any subsumes int" $
      inferDefinition empty (uDefinition (Identifier "some-number")
                                                (Just $ implicit (tsSymbol (Identifier "any")))
                                                (uLiteral (uInt 1)))
      `shouldSucceedWith`
      tDefinition (Identifier "some-number") (forall [] typeAny, tLiteral (tInt 1) typeInt)


  it "infers twice function with correct type signature" $
    inferDefinition
    empty
    (uDefinition
     (Identifier "twice")
     (Just $ explicit [varBinding "a"] (tsFn (tsFn (tsVar "a") (tsVar "a")) (tsFn (tsVar "a") (tsVar "a"))))
     twiceUntyped)
    `shouldSucceedWith`
    twiceTyped

  it "fails on twice function with incorrect type signature" $
    shouldFail $
      inferDefinition empty (uDefinition (Identifier "twice")
                                                (Just $ explicit [varBinding "a"] (tsFn (tsVar "a") (tsVar "a")))
                                                twiceUntyped)

  it "infers recursive definition" $
    inferDefinition
    predef
    (uDefinition
     (Identifier "f")
     (Just $ implicit (tsFn (tsSymbol (Identifier "int")) (tsSymbol (Identifier "int"))))
     countToZero)
    `shouldSucceedWith`
    countToZeroTyped

  it "infers recursive definition without type signature" $
    inferDefinition predef (uDefinition (Identifier "f") Nothing countToZero)
    `shouldSucceedWith`
    countToZeroTyped

  it "fails on recursive with incorrect signature" $
    shouldFail $
      inferDefinition
        predef
        (uDefinition
         (Identifier "f")
         (Just $ implicit (tsFn (tsSymbol (Identifier "int")) (tsSymbol (Identifier "any")))) countToZero)
