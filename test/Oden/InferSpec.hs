module Oden.InferSpec where

import           Test.Hspec

import qualified Data.Set              as Set

import           Oden.Core.Typed
import           Oden.Core.Expr
import           Oden.Core.Untyped
import           Oden.Environment
import           Oden.Identifier
import           Oden.Infer            (inferExpr)
import           Oden.Predefined
import           Oden.Pretty           ()
import           Oden.QualifiedName    (nameInUniverse)
import           Oden.Type.Polymorphic

import           Oden.Assertions
import           Oden.Infer.Fixtures

untypedSymbol s = Symbol missing (Identifier s) Untyped
untypedInt n = Literal missing (Int n) Untyped

untypedFalse = Literal missing (Bool False) Untyped
untypedTrue = Literal missing (Bool True) Untyped

untypedNoArgFn body = NoArgFn missing body Untyped
untypedFn p body = Fn missing (NameBinding missing (Identifier p)) body Untyped

untypedNoArgApplication f = NoArgApplication missing f Untyped
untypedApplication f a = Application missing f a Untyped

spec :: Spec
spec = describe "inferExpr" $ do
  it "infers int literal" $
    inferExpr empty (untypedInt 1)
    `shouldSucceedWith`
    (scheme typeInt,
     tLiteral (tInt 1) typeInt)

  it "infers int slice" $
    inferExpr empty (Slice missing [untypedInt 1] Untyped)
    `shouldSucceedWith`
    (scheme intSlice,
     tSlice [tLiteral (tInt 1) typeInt] intSlice)

  it "fails on mixed type slice" $
    shouldFail $
      inferExpr empty (Slice
                       missing
                       [untypedInt 1,
                        Literal missing (String "foo") Untyped]
                       Untyped)

  it "infers tuple" $
    let tupleType = TTuple missing typeInt typeString [typeUnit]
    in
      inferExpr
      empty
      (Tuple
       missing
       (untypedInt 1)
       (Literal missing (String "foo") Untyped)
       [Literal missing Unit Untyped]
       Untyped)
      `shouldSucceedWith`
      (scheme tupleType,
      tTuple
      (tLiteral (tInt 1) typeInt)
      (tLiteral (tString "foo") typeString)
      [tLiteral tUnit typeUnit]
      tupleType)

  it "infers record member access" $
    inferExpr
    empty
    (untypedFn
     "x"
     (MemberAccess
      missing
      (NamedMemberAccess
       (untypedSymbol "x")
       (Identifier "y"))
      Untyped))
    `shouldSucceedWith`
    let recordType = TRecord missing (RExtension missing (Identifier "y") tvarA tvarB) in
      (scheme (typeFn recordType tvarA),
       tFn
       (tNameBinding (Identifier "x"))
       (MemberAccess
        missing
        (RecordFieldAccess (tSymbol (Identifier "x") recordType) (Identifier "y"))
        tvarA)
       (typeFn recordType tvarA))

  it "infers multiple record member accesses" $
    inferExpr
    empty
    (untypedFn "x"
     (Tuple
      missing
      (MemberAccess
       missing
       (NamedMemberAccess
        (untypedSymbol "x")
        (Identifier "y"))
       Untyped)
      (MemberAccess
       missing
       (NamedMemberAccess
        (untypedSymbol "x")
        (Identifier "z"))
       Untyped)
      []
      Untyped))
    `shouldSucceedWith'`
    let recordType = TRecord missing (RExtension missing (Identifier "y") tvarA (RExtension missing (Identifier "z") tvarC tvarB))
        tupleType = TTuple missing tvarA tvarC [] in
      (scheme (typeFn recordType tupleType),
      tFn
      (tNameBinding (Identifier "x"))
      (Tuple
       missing
       (MemberAccess
        missing
        (RecordFieldAccess
         (tSymbol (Identifier "x") recordType)
         (Identifier "y"))
        tvarA)
       (MemberAccess
        missing
        (RecordFieldAccess
         (tSymbol (Identifier "x") recordType)
         (Identifier "z"))
        tvarC)
       []
       tupleType)
      (typeFn recordType (TTuple missing tvarA tvarC [])))

  it "infers package member access" $
    inferExpr
    fooBarPkgEnv
    (MemberAccess
     missing
     (NamedMemberAccess
      (untypedSymbol "foo")
      (Identifier "Bar"))
     Untyped)
    `shouldSucceedWith`
    (scheme typeInt,
     MemberAccess
     missing
     (PackageMemberAccess
      (Identifier "foo")
      (Identifier "Bar"))
     typeInt)

  it "infers identity fn" $
    inferExpr empty (untypedFn "x" (untypedSymbol "x"))
    `shouldSucceedWith`
    (scheme (typeFn tvarA tvarA),
     tFn (tNameBinding (Identifier "x")) (tSymbol (Identifier "x") tvarA) (typeFn tvarA tvarA))

  it "infers no-arg fn" $
    inferExpr empty (untypedNoArgFn untypedTrue)
    `shouldSucceedWith`
    (scheme (typeNoArgFn typeBool),
     tNoArgFn (tLiteral (tBool True) typeBool) (typeNoArgFn typeBool))

  it "infers no-arg fn application" $
    inferExpr empty (untypedNoArgApplication (untypedNoArgFn untypedTrue))
    `shouldSucceedWith`
    (scheme typeBool,
     tNoArgApplication (tNoArgFn (tLiteral (tBool True) typeBool) (typeNoArgFn typeBool)) typeBool)

  it "infers multi-arg fn application" $
    inferExpr empty (untypedApplication
                     (untypedApplication
                      (untypedFn "x" (untypedFn "y" (untypedInt 1)))
                      untypedFalse)
                     untypedTrue)
    `shouldSucceedWith`
    (scheme typeInt,
     tApplication
     (tApplication
      (tFn (tNameBinding (Identifier "x"))
       (tFn (tNameBinding (Identifier "y")) (tLiteral (tInt 1) typeInt) (typeBool `typeFn` typeInt))
       (typeBool `typeFn` (typeBool `typeFn` typeInt)))
      (tLiteral (tBool False) typeBool)
      (typeBool `typeFn` typeInt))
     (tLiteral (tBool True) typeBool)
     typeInt)

  it "fails in fn application with type mismatch" $
    shouldFail $
      inferExpr
      predefAndStringLength
      (untypedApplication
       (untypedSymbol "stringLength")
       (untypedInt 1))

  it "infers 1 + 1" $
    inferExpr
    predef
    (Application
     missing
     (Application
      missing
      (MethodReference missing (NamedMethodReference (Identifier "Addition") (Identifier "Add")) Untyped)
      (untypedInt 1)
      Untyped)
      (untypedInt 1)
     Untyped)
    `shouldSucceedWith`
    let constraint = ProtocolConstraint missing (nameInUniverse "Addition") typeInt in
    (scheme typeInt,
     Application
     missing
     (Application
      missing
      (MethodReference
       missing
       (Unresolved (nameInUniverse "Addition") (Identifier "Add") constraint)
       (TConstrained (Set.singleton constraint) (typeFn typeInt (typeFn typeInt typeInt))))
      (tLiteral (tInt 1) typeInt)
      (typeFn typeInt typeInt))
     (tLiteral (tInt 1) typeInt)
     typeInt)

  it "infers let" $
    inferExpr
    empty
    (Let
     missing
     (NameBinding missing (Identifier "x"))
     (untypedInt 1)
     (untypedSymbol "x")
     Untyped)
    `shouldSucceedWith`
    (scheme typeInt,
     tLet (tNameBinding (Identifier "x")) (tLiteral (tInt 1) typeInt) (tSymbol (Identifier "x") typeInt) typeInt)

  it "infers let with shadowing" $
    inferExpr
    empty
    (Let
     missing
     (NameBinding missing (Identifier "x"))
     (untypedInt 1)
     (Let
      missing
      (NameBinding missing (Identifier "x"))
      (untypedSymbol "x")
      (untypedSymbol "x")
      Untyped)
     Untyped)
    `shouldSucceedWith`
    (scheme typeInt,
     tLet
      (tNameBinding (Identifier "x"))
      (tLiteral (tInt 1) typeInt)
      (tLet
       (tNameBinding (Identifier "x"))
       (tSymbol (Identifier "x") typeInt)
       (tSymbol (Identifier "x") typeInt)
       typeInt)
      typeInt)

  it "infers polymorphic if" $
    inferExpr
    empty
    (untypedFn "x"
     (If
      missing
      (Literal missing (Bool True) Untyped)
      (untypedSymbol "x")
      (untypedSymbol "x")
      Untyped))
    `shouldSucceedWith`
    (scheme (typeFn tvarA tvarA),
     tFn (tNameBinding (Identifier "x")) (tIf (tLiteral (tBool True) typeBool)
                          (tSymbol (Identifier "x") tvarA)
                          (tSymbol (Identifier "x") tvarA)
                          tvarA) (typeFn tvarA tvarA))

  it "infers single-arg foreign func application" $
    inferExpr
    predef
    (Application
     missing
     (untypedSymbol "len")
     (Slice missing [Literal missing (Bool True) Untyped] Untyped)
     Untyped)
    `shouldSucceedWith`
    (scheme typeInt,
     tApplication
     (tFn (tNameBinding (Identifier "_g0"))
      (tForeignFnApplication
       (Symbol missing (Identifier "len") (typeForeign False [TSlice predefined typeBool] [typeInt]))
       [tSymbol (Identifier "_g0") (TSlice predefined typeBool)]
       typeInt)
      (typeFn (TSlice predefined typeBool) typeInt))
     (Slice missing [Literal missing (tBool True) typeBool] (typeSlice typeBool))
     typeInt)

  it "infers multi-arg foreign func application" $
    inferExpr
    predefAndMax
    (Application
     missing
     (Application
      missing
      (untypedSymbol "max")
      (untypedInt 0)
      Untyped)
     (untypedInt 1)
     Untyped)
    `shouldSucceedWith`
    (scheme typeInt,
     tApplication
     (tApplication
      (tFn
       (tNameBinding (Identifier "_g0"))
       (tFn
        (tNameBinding (Identifier "_g1"))
        (tForeignFnApplication
         (tSymbol (Identifier "max") (typeForeign False [typeInt, typeInt] [typeInt]))
         [tSymbol (Identifier "_g0") typeInt, tSymbol (Identifier "_g1") typeInt]
         typeInt)
        (typeFn typeInt typeInt))
       (typeFn typeInt (typeFn typeInt typeInt)))
      (tLiteral (tInt 0) typeInt)
      (typeFn typeInt typeInt))
     (tLiteral (tInt 1) typeInt)
     typeInt)

  it "infers variadic func application" $
    inferExpr
    predefAndMaxVariadic
    (Application
     missing
     (untypedSymbol "max")
     (Slice
       missing
       [untypedInt 0
       ,untypedInt 1]
       Untyped)
     Untyped)
    `shouldSucceedWith`
    (scheme typeInt,
     tApplication
     (tFn
      (tNameBinding (Identifier "_g0"))
      (tForeignFnApplication
       (tSymbol (Identifier "max") (typeForeign True [TSlice missing typeInt] [typeInt]))
       [tSymbol (Identifier "_g0") (TSlice missing typeInt)]
       typeInt)
      (typeFn (TSlice missing typeInt) typeInt))
     (tSlice [tLiteral (tInt 0) typeInt, tLiteral (tInt 1) typeInt] (TSlice missing typeInt))
     typeInt)

  it "infers variadic no-arg func application" $
    inferExpr
    predefAndMaxVariadic
    (Application
     missing
     (untypedSymbol "max")
     (Slice missing [] Untyped)
     Untyped)
    `shouldSucceedWith`
    (scheme typeInt,
     tApplication
     (tFn
      (tNameBinding (Identifier "_g0"))
      (tForeignFnApplication
       (tSymbol (Identifier "max") (typeForeign True [TSlice missing typeInt] [typeInt]))
       [tSymbol (Identifier "_g0") (TSlice missing typeInt)]
       typeInt)
      (typeFn (TSlice missing typeInt) typeInt))
     (tSlice [] (TSlice missing typeInt))
     typeInt)

  it "infers record initializer" $
    let recordType = TRecord missing (RExtension missing (Identifier "msg") typeString (REmpty missing)) in
      inferExpr predef (RecordInitializer
                        missing
                        [FieldInitializer
                         missing
                         (Identifier "msg")
                         (Literal missing (String "hello") Untyped)]
                        Untyped)
      `shouldSucceedWith`
      (scheme recordType,
       RecordInitializer
       missing
       [FieldInitializer
        missing
        (Identifier "msg")
        (Literal missing (String "hello") typeString)]
       recordType)

  it "infers record field access fn" $
    let recordType = typeRecord (rowExt (Identifier "a") tvarA tvarB)
        functionType = typeFn recordType tvarA in
    inferExpr empty (untypedFn "x"
                     (MemberAccess
                      missing
                      (NamedMemberAccess
                       (untypedSymbol "x")
                       (Identifier "a"))
                      Untyped))
    `shouldSucceedWith`
    (scheme functionType,
     tFn
     (tNameBinding (Identifier "x"))
     (MemberAccess
      missing
      (RecordFieldAccess (tSymbol (Identifier "x") recordType) (Identifier "a"))
      tvarA)
     functionType)
