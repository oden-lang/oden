module Oden.InferSpec where

import           Test.Hspec

import qualified Oden.Core             as Core
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Env
import           Oden.Identifier
import           Oden.Infer
import           Oden.Predefined
import           Oden.Type.Polymorphic

import           Oden.Assertions

intSlice :: Type
intSlice = TSlice typeInt

predef :: Env
predef = fromScope predefined

predefAndMax :: Env
predefAndMax = predef `extend` (Unqualified "max",
                                Forall [] (TUncurriedFn [typeInt, typeInt] typeInt))

predefAndMaxVariadic :: Env
predefAndMaxVariadic = predef `extend` (Unqualified "max",
                                        Forall [] (TVariadicFn [] typeInt typeInt))

predefAndIdentityAny :: Env
predefAndIdentityAny = predef `extend` (Unqualified "identity",
                                        Forall [] (TUncurriedFn [TAny] TAny))

booleanOp :: Type
booleanOp = typeBool `TFn` (typeBool `TFn` typeBool)

tvA :: TVar
tvA = TV "a"

tvarA :: Type
tvarA = TVar (TV "a")

countToZero :: Untyped.Expr
countToZero =
  Untyped.Fn
  "x"
  (Untyped.If
   (Untyped.Application
    (Untyped.Application
     (Untyped.Symbol (Unqualified "=="))
     [Untyped.Symbol (Unqualified "x")])
    [Untyped.Literal (Untyped.Int 0)])
   (Untyped.Literal (Untyped.Int 0))
   (Untyped.Application
    (Untyped.Symbol (Unqualified "f"))
    [Untyped.Application
     (Untyped.Application
      (Untyped.Symbol (Unqualified "-"))
      [Untyped.Symbol (Unqualified "x")])
     [Untyped.Literal (Untyped.Int 1)]]))

intToInt :: Type
intToInt = TFn typeInt typeInt

intToIntToInt :: Type
intToIntToInt = TFn typeInt (TFn typeInt typeInt)

countToZeroTyped :: Core.Definition
countToZeroTyped =
  Core.Definition
   "f"
   (Forall [] (TFn typeInt typeInt),
    Core.Fn
    "x"
    (Core.If
     (Core.Application
      (Core.Application
       (Core.Symbol (Unqualified "==") (TFn typeInt (TFn typeInt typeBool)))
       (Core.Symbol (Unqualified "x") typeInt)
       (TFn typeInt typeBool))
      (Core.Literal (Core.Int 0) typeInt)
      typeBool)
     (Core.Literal (Core.Int 0) typeInt)
     (Core.Application
      (Core.Symbol (Unqualified "f") intToInt)
      (Core.Application
       (Core.Application
        (Core.Symbol (Unqualified "-") intToIntToInt)
        (Core.Symbol (Unqualified "x") typeInt)
        intToInt)
       (Core.Literal (Core.Int 1) typeInt)
       typeInt)
      typeInt)
     typeInt)
    intToInt)

twiceUntyped :: Untyped.Expr
twiceUntyped =
  Untyped.Fn
   "f"
   (Untyped.Fn
   "x"
   (Untyped.Application
     (Untyped.Symbol (Unqualified "f"))
     [Untyped.Application
     (Untyped.Symbol (Unqualified "f"))
     [Untyped.Symbol (Unqualified "x")]]))

twiceTyped :: Core.Definition
twiceTyped =
  Core.Definition "twice" (Forall [tvA] (TFn (TFn tvarA tvarA) (TFn tvarA tvarA)),
                           Core.Fn
                           "f"
                           (Core.Fn
                           "x"
                           (Core.Application
                             (Core.Symbol (Unqualified "f") (TFn tvarA tvarA))
                             (Core.Application
                             (Core.Symbol (Unqualified "f") (TFn tvarA tvarA))
                             (Core.Symbol (Unqualified "x") tvarA)
                             tvarA)
                             tvarA)
                           (TFn tvarA tvarA))
                           (TFn (TFn tvarA tvarA) (TFn tvarA tvarA)))

spec :: Spec
spec = do
  describe "inferExpr" $ do
    it "infers int literal" $
      inferExpr empty (Untyped.Literal (Untyped.Int 1))
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.Literal (Core.Int 1) typeInt)

    it "infers int slice" $
      inferExpr empty (Untyped.Slice [Untyped.Literal (Untyped.Int 1)])
      `shouldSucceedWith`
      (Forall [] intSlice,
       Core.Slice [Core.Literal (Core.Int 1) typeInt] intSlice)

    it "fails on mixed type slice" $
      shouldFail $
        inferExpr empty (Untyped.Slice [Untyped.Literal (Untyped.Int 1),
                                        Untyped.Literal (Untyped.String "foo")])

    it "infers identity fn" $
      inferExpr empty (Untyped.Fn "x" (Untyped.Symbol (Unqualified "x")))
      `shouldSucceedWith`
      (Forall [TV "a"] (TFn (TVar (TV "a")) (TVar (TV "a"))),
       Core.Fn "x" (Core.Symbol (Unqualified "x") (TVar (TV "a"))) (TFn (TVar (TV "a")) (TVar (TV "a"))))

    it "infers no-arg fn" $
      inferExpr empty (Untyped.NoArgFn (Untyped.Literal (Untyped.Bool True)))
      `shouldSucceedWith`
      (Forall [] (TNoArgFn typeBool),
       Core.NoArgFn (Core.Literal (Core.Bool True) typeBool) (TNoArgFn typeBool))

    it "infers no-arg fn application" $
      inferExpr empty (Untyped.Application (Untyped.NoArgFn (Untyped.Literal (Untyped.Bool True))) [])
      `shouldSucceedWith`
      (Forall [] typeBool,
       (Core.NoArgApplication (Core.NoArgFn (Core.Literal (Core.Bool True) typeBool) (TNoArgFn typeBool))) typeBool)

    it "infers multi-arg fn application" $
      inferExpr empty (Untyped.Application
                       (Untyped.Fn "x" (Untyped.Fn "y" (Untyped.Literal (Untyped.Int 1))))
                       [Untyped.Literal (Untyped.Bool False), Untyped.Literal (Untyped.Bool False)])
      `shouldSucceedWith`
      (Forall [] typeInt,
       (Core.Application
        (Core.Application
         (Core.Fn "x"
          (Core.Fn "y" (Core.Literal (Core.Int 1) typeInt) (typeBool `TFn` typeInt))
          (typeBool `TFn` (typeBool `TFn` typeInt)))
         (Core.Literal (Core.Bool False) typeBool)
         (typeBool `TFn` typeInt))
        (Core.Literal (Core.Bool False) typeBool)
        typeInt))

    it "infers nested fn application" $
      inferExpr
      predef
      (Untyped.Application
       (Untyped.Symbol (Unqualified "or"))
       [Untyped.Application
        (Untyped.Symbol (Unqualified "and"))
        [Untyped.Literal (Untyped.Bool False),
         Untyped.Literal (Untyped.Bool False)],
        Untyped.Literal (Untyped.Bool True)])
      `shouldSucceedWith`
      (Forall [] typeBool,
       (Core.Application
        (Core.Application
         (Core.Symbol (Unqualified "or") booleanOp)
         (Core.Application
          (Core.Application
           (Core.Symbol (Unqualified "and") booleanOp)
           (Core.Literal (Core.Bool False) typeBool)
           (typeBool `TFn` typeBool))
           (Core.Literal (Core.Bool False) typeBool)
           typeBool)
         (typeBool `TFn` typeBool))
        (Core.Literal (Core.Bool True) typeBool))
       typeBool)

    it "infers fn application with any-type" $
      inferExpr
        predefAndIdentityAny
        (Untyped.Application
         (Untyped.Symbol (Unqualified "identity"))
         [Untyped.Literal (Untyped.Bool False)])
      `shouldSucceedWith`
      (Forall [] TAny,
       Core.UncurriedFnApplication
        (Core.Symbol (Unqualified "identity") (TUncurriedFn [TAny] TAny))
        [Core.Literal (Core.Bool False) typeBool]
        TAny)

    it "infers (+ 1 1)" $
      inferExpr
        predef
        (Untyped.Application
         (Untyped.Symbol (Unqualified "+"))
         [Untyped.Literal (Untyped.Int 1)
         ,Untyped.Literal (Untyped.Int 1)])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.UncurriedFnApplication
        (Core.Symbol (Unqualified "+") (TUncurriedFn [typeInt, typeInt] typeInt))
        [(Core.Literal (Core.Int 1) typeInt)
        ,(Core.Literal (Core.Int 1) typeInt)]
        typeInt)

    it "infers fn application with any-type with multiple \"instances\"" $
      inferExpr
        predefAndIdentityAny
        (Untyped.Application
         (Untyped.Symbol (Unqualified "identity"))
         [Untyped.Application
          (Untyped.Symbol (Unqualified "identity"))
          [Untyped.Literal (Untyped.Bool False)]])
      `shouldSucceedWith`
      (Forall [] TAny,
       Core.UncurriedFnApplication
        (Core.Symbol (Unqualified "identity") (TUncurriedFn [TAny] TAny))
        [Core.UncurriedFnApplication
         (Core.Symbol (Unqualified "identity") (TUncurriedFn [TAny] TAny))
         [Core.Literal (Core.Bool False) typeBool]
         TAny]
        TAny)

    it "infers let" $
      inferExpr empty (Untyped.Let "x" (Untyped.Literal (Untyped.Int 1)) (Untyped.Symbol (Unqualified "x")))
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.Let "x" (Core.Literal (Core.Int 1) typeInt) (Core.Symbol (Unqualified "x") typeInt) typeInt)

    it "infers let with shadowing" $
      inferExpr empty (Untyped.Let
                       "x"
                       (Untyped.Literal (Untyped.Int 1))
                       (Untyped.Let
                        "x"
                        (Untyped.Symbol (Unqualified "x"))
                        (Untyped.Symbol (Unqualified "x"))))
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.Let
        "x"
        (Core.Literal (Core.Int 1) typeInt)
        (Core.Let
         "x"
         (Core.Symbol (Unqualified "x") typeInt)
         (Core.Symbol (Unqualified "x") typeInt)
         typeInt)
        typeInt)

    it "infers polymorphic if" $
      inferExpr empty (Untyped.Fn "x" (Untyped.If (Untyped.Literal (Untyped.Bool True)) (Untyped.Symbol (Unqualified "x")) (Untyped.Symbol (Unqualified "x"))))
      `shouldSucceedWith`
      (Forall [TV "a"] (TFn (TVar (TV "a")) (TVar (TV "a"))),
       Core.Fn "x" (Core.If (Core.Literal (Core.Bool True) typeBool)
                            (Core.Symbol (Unqualified "x") (TVar (TV "a")))
                            (Core.Symbol (Unqualified "x") (TVar (TV "a")))
                            (TVar (TV "a"))) (TFn (TVar (TV "a")) (TVar (TV "a"))))

    it "infers single-arg uncurried func application" $
      inferExpr predef (Untyped.Application (Untyped.Symbol (Unqualified "len")) [Untyped.Slice [Untyped.Literal (Untyped.Bool True)]])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.UncurriedFnApplication (Core.Symbol (Unqualified "len") (TUncurriedFn [TSlice typeBool] typeInt))
                              [Core.Slice [Core.Literal (Core.Bool True) typeBool] (TSlice typeBool)]
       typeInt)

    it "infers single-arg uncurried func application" $
      inferExpr predefAndMax (Untyped.Application (Untyped.Symbol (Unqualified "max"))
                                                  [Untyped.Literal (Untyped.Int 0)
                                                  ,Untyped.Literal (Untyped.Int 1)])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.UncurriedFnApplication (Core.Symbol (Unqualified "max") (TUncurriedFn [typeInt, typeInt] typeInt))
                              [Core.Literal (Core.Int 0) typeInt
                              ,Core.Literal (Core.Int 1) typeInt]
       typeInt)

    it "infers variadic func application" $
      inferExpr predefAndMaxVariadic (Untyped.Application (Untyped.Symbol (Unqualified "max"))
                                                          [Untyped.Literal (Untyped.Int 0)
                                                          ,Untyped.Literal (Untyped.Int 1)])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.UncurriedFnApplication (Core.Symbol (Unqualified "max") (TVariadicFn [] typeInt typeInt))
                              [Core.Slice [Core.Literal (Core.Int 0) typeInt
                                          ,Core.Literal (Core.Int 1) typeInt] typeInt]
       typeInt)

    it "infers variadic no-arg func application" $
      inferExpr predefAndMaxVariadic (Untyped.Application (Untyped.Symbol (Unqualified "max")) [])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.UncurriedFnApplication (Core.Symbol (Unqualified "max") (TVariadicFn [] typeInt typeInt))
                              [Core.Slice [] typeInt]
       typeInt)

  describe "inferDefinition" $ do

    it "infers (def n (+ 1 1))" $
      inferDefinition predef (Untyped.Definition "n" Nothing (Untyped.Application
                                                              (Untyped.Symbol (Unqualified "+"))
                                                              [Untyped.Literal (Untyped.Int 1)
                                                              ,Untyped.Literal (Untyped.Int 1)]))
      `shouldSucceedWith`
      Core.Definition
      "n"
      (Forall [] typeInt,
       Core.UncurriedFnApplication
        (Core.Symbol (Unqualified "+") (TUncurriedFn [typeInt, typeInt] typeInt))
        [(Core.Literal (Core.Int 1) typeInt)
        ,(Core.Literal (Core.Int 1) typeInt)]
        typeInt)

    it "infers definition without type signature" $
      inferDefinition empty (Untyped.Definition "x" Nothing (Untyped.Literal (Untyped.Int 1)))
      `shouldSucceedWith`
      Core.Definition "x" (Forall [] typeInt, Core.Literal (Core.Int 1) typeInt)

    it "infers polymorphic definition without type signature" $
      shouldSucceed $
        inferDefinition
          empty
          (Untyped.Definition "id"
                              Nothing
                              (Untyped.Fn "x" (Untyped.Symbol (Unqualified "x"))))

    it "infers definition with type signature" $
      inferDefinition empty (Untyped.Definition "x" (Just $ Forall [] TAny) (Untyped.Literal (Untyped.Int 1)))
      `shouldSucceedWith`
      Core.Definition "x" (Forall [] TAny, Core.Literal (Core.Int 1) typeInt)

    it "infers polymorphic definition with type signature" $
      inferDefinition empty (Untyped.Definition "id"
                                                (Just $ Forall [TV "a"] (TFn (TVar (TV "a")) (TVar (TV "a"))))
                                                (Untyped.Fn "x" (Untyped.Symbol (Unqualified "x"))))
      `shouldSucceedWith`
      Core.Definition "id" (Forall [TV "a"] (TFn (TVar (TV "a")) (TVar (TV "a"))),
                            Core.Fn "x" (Core.Symbol (Unqualified "x") (TVar (TV "a"))) (TFn (TVar (TV "a")) (TVar (TV "a"))))

    it "fails when specified type signature does not unify" $
      shouldFail $
        inferDefinition empty (Untyped.Definition "some-number"
                                                  (Just $ Forall [] typeBool)
                                                  (Untyped.Literal (Untyped.Int 1)))

    it "subsumes int with any" $
        inferDefinition empty (Untyped.Definition "some-number"
                                                  (Just $ Forall [] TAny)
                                                  (Untyped.Literal (Untyped.Int 1)))
        `shouldSucceedWith`
        Core.Definition "some-number" (Forall [] TAny, Core.Literal (Core.Int 1) typeInt)


    it "infers twice function with correct type signature" $
      inferDefinition empty (Untyped.Definition "twice"
                                                (Just $ Forall [tvA] (TFn (TFn tvarA tvarA) (TFn tvarA tvarA)))
                                                twiceUntyped)
      `shouldSucceedWith`
      twiceTyped

    it "fails on twice function with incorrect type signature" $
      shouldFail $
        inferDefinition empty (Untyped.Definition "twice"
                                                  (Just $ Forall [tvA] (TFn tvarA tvarA))
                                                  twiceUntyped)

    it "infers recursive definition" $
      inferDefinition predef (Untyped.Definition "f" (Just $ Forall [] intToInt) countToZero)
      `shouldSucceedWith`
      countToZeroTyped

    it "infers recursive definition without type signature" $
      inferDefinition predef (Untyped.Definition "f" Nothing countToZero)
      `shouldSucceedWith`
      countToZeroTyped

    it "fails on recursive with incorrect signature" $
      shouldFail $
        inferDefinition predef (Untyped.Definition "f" (Just $ Forall [] (TFn typeInt TAny)) countToZero)
