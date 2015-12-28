module Oden.InferSpec where

import           Test.Hspec

import qualified Oden.Core             as Core
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Env
import           Oden.Identifier
import           Oden.Infer
import           Oden.Type.Polymorphic

import           Oden.Assertions

spec :: Spec
spec = do
  describe "inferExpr" $ do
    it "infers int literal" $
      inferExpr empty (Untyped.Literal (Untyped.Int 1))
      `shouldSucceedWith`
      (Forall [] typeInt,
       Core.Literal (Core.Int 1) typeInt)

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
      inferExpr empty (Untyped.NoArgApplication (Untyped.NoArgFn (Untyped.Literal (Untyped.Bool True))))
      `shouldSucceedWith`
      (Forall [] typeBool,
       (Core.NoArgApplication (Core.NoArgFn (Core.Literal (Core.Bool True) typeBool) (TArrSingle typeBool))) typeBool)

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
