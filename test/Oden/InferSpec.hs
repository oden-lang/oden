module Oden.InferSpec where

import           Test.Hspec

import qualified Oden.Core             as Core
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Env
import           Oden.Predefined
import           Oden.Identifier
import           Oden.Infer
import           Oden.Type.Polymorphic

import           Oden.Assertions

intSlice :: Type
intSlice = TSlice typeInt

predef :: Env
predef = fromScope predefined

predefAndMax :: Env
predefAndMax = predef `extend` (Unqualified "max",
                                Forall [] (TGoFunc [typeInt, typeInt] typeInt))

predefAndMaxVariadic :: Env
predefAndMaxVariadic = predef `extend` (Unqualified "max",
                                        Forall [] (TVariadicGoFunc [] typeInt typeInt))

spec :: Spec
spec =
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
      (Forall [TV "a"] (TArr (TVar (TV "a")) (TVar (TV "a"))),
       Core.Fn "x" (Core.Symbol (Unqualified "x") (TVar (TV "a"))) (TArr (TVar (TV "a")) (TVar (TV "a"))))

    it "infers no-arg fn" $
      inferExpr empty (Untyped.NoArgFn (Untyped.Literal (Untyped.Bool True)))
      `shouldSucceedWith`
      (Forall [] (TArrSingle typeBool),
       Core.NoArgFn (Core.Literal (Core.Bool True) typeBool) (TArrSingle typeBool))

    it "infers no-arg fn application" $
      inferExpr empty (Untyped.Application (Untyped.NoArgFn (Untyped.Literal (Untyped.Bool True))) [])
      `shouldSucceedWith`
      (Forall [] typeBool,
       (Core.NoArgApplication (Core.NoArgFn (Core.Literal (Core.Bool True) typeBool) (TArrSingle typeBool))) typeBool)

    it "infers multi-arg fn application" $
      inferExpr empty (Untyped.Application
                       (Untyped.Fn "x" (Untyped.Fn "y" (Untyped.Literal (Untyped.Int 1))))
                       [Untyped.Literal (Untyped.Bool False), Untyped.Literal (Untyped.Bool False)])
      `shouldSucceedWith`
      (Forall [] typeInt,
       (Core.Application
        (Core.Application
         (Core.Fn "x"
          (Core.Fn "y" (Core.Literal (Core.Int 1) typeInt) (typeBool `TArr` typeInt))
          (typeBool `TArr` (typeBool `TArr` typeInt)))
         (Core.Literal (Core.Bool False) typeBool)
         (typeBool `TArr` typeInt))
        (Core.Literal (Core.Bool False) typeBool)
        typeInt))

    it "infers let" $
      inferExpr empty (Untyped.Let "x" (Untyped.Literal (Untyped.Int 1)) (Untyped.Symbol (Unqualified "x")))
      `shouldSucceedWith`
      (Forall [] typeInt,
       (Core.Let "x" (Core.Literal (Core.Int 1) typeInt) (Core.Symbol (Unqualified "x") typeInt) typeInt))

    it "infers polymorphic if" $
      inferExpr empty (Untyped.Fn "x" (Untyped.If (Untyped.Literal (Untyped.Bool True)) (Untyped.Symbol (Unqualified "x")) (Untyped.Symbol (Unqualified "x"))))
      `shouldSucceedWith`
      (Forall [TV "a"] (TArr (TVar (TV "a")) (TVar (TV "a"))),
       Core.Fn "x" (Core.If (Core.Literal (Core.Bool True) typeBool)
                            (Core.Symbol (Unqualified "x") (TVar (TV "a")))
                            (Core.Symbol (Unqualified "x") (TVar (TV "a")))
                            (TVar (TV "a"))) (TArr (TVar (TV "a")) (TVar (TV "a"))))

    it "infers single-arg go func application" $
      inferExpr predef (Untyped.Application (Untyped.Symbol (Unqualified "len")) [Untyped.Slice [Untyped.Literal (Untyped.Bool True)]])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.GoFuncApplication (Core.Symbol (Unqualified "len") (TGoFunc [TSlice typeBool] typeInt))
                              [Core.Slice [Core.Literal (Core.Bool True) typeBool] (TSlice typeBool)]
       typeInt)

    it "infers single-arg go func application" $
      inferExpr predefAndMax (Untyped.Application (Untyped.Symbol (Unqualified "max"))
                                                  [Untyped.Literal (Untyped.Int 0)
                                                  ,Untyped.Literal (Untyped.Int 1)])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.GoFuncApplication (Core.Symbol (Unqualified "max") (TGoFunc [typeInt, typeInt] typeInt))
                              [Core.Literal (Core.Int 0) typeInt
                              ,Core.Literal (Core.Int 1) typeInt]
       typeInt)

    it "infers variadic go func application" $
      inferExpr predefAndMaxVariadic (Untyped.Application (Untyped.Symbol (Unqualified "max"))
                                                          [Untyped.Literal (Untyped.Int 0)
                                                          ,Untyped.Literal (Untyped.Int 1)])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.GoFuncApplication (Core.Symbol (Unqualified "max") (TVariadicGoFunc [] typeInt typeInt))
                              [Core.Slice [Core.Literal (Core.Int 0) typeInt
                                          ,Core.Literal (Core.Int 1) typeInt] typeInt]
       typeInt)

    it "infers variadic no-arg go func application" $
      inferExpr predefAndMaxVariadic (Untyped.Application (Untyped.Symbol (Unqualified "max")) [])
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.GoFuncApplication (Core.Symbol (Unqualified "max") (TVariadicGoFunc [] typeInt typeInt))
                              [Core.Slice [] typeInt]
       typeInt)
