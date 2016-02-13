module Oden.ExplodeSpec where

import qualified Oden.Core.Untyped     as U
import           Oden.Explode
import           Oden.Identifier
import           Oden.SourceInfo
import           Oden.Syntax
import           Oden.Type.Polymorphic
import           Test.Hspec

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<test>" l c)

spec :: Spec
spec = do
  describe "explodeExpr" $ do
    it "converts symbol" $
      explodeExpr (Symbol (src 1 1) (Unqualified "x"))
      `shouldBe`
      U.Symbol (src 1 1) (Unqualified "x")

    it "converts int literal" $
      explodeExpr (Literal (src 1 1) (Int 1))
      `shouldBe`
      U.Literal (src 1 1) (U.Int 1)

    it "converts bool literal" $
      explodeExpr (Literal (src 1 1) (Bool True))
      `shouldBe`
      U.Literal (src 1 1) (U.Bool True)

    it "converts fn application with no params" $
      explodeExpr (Application
                   (src 1 1)
                   (Fn (src 1 1) [] (Symbol (src 1 3) (Unqualified "x")))
                   [])
      `shouldBe`
      U.Application
      (src 1 1)
      (U.NoArgFn (src 1 1) (U.Symbol (src 1 3) (Unqualified "x"))) []

    it "converts fn application with multiple params" $
      explodeExpr (Application
                   (src 1 1)
                   (Fn
                    (src 1 1)
                    [Binding (src 1 2) "x",
                     Binding (src 1 3) "y"]
                    (Symbol (src 1 4) (Unqualified "x")))
                   [Symbol (src 1 5) (Unqualified "x"),
                    Symbol (src 1 9) (Unqualified "y")])
      `shouldBe`
      U.Application
      (src 1 1)
      (U.Fn
       (src 1 1)
       (U.Binding (src 1 2) "x")
       (U.Fn
        (src 1 1)
        (U.Binding (src 1 3) "y")
        (U.Symbol (src 1 4) (Unqualified "x"))))
      [U.Symbol (src 1 5) (Unqualified "x"),
       U.Symbol (src 1 9) (Unqualified "y")]

  describe "explodeTopLevel" $ do
    it "converts fn definition with no argument" $
      (snd <$> explodeTopLevel [FnDefinition (src 1 1) "f" [] (Symbol (src 1 3) (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition (src 1 1) "f" Nothing (U.NoArgFn (src 1 1) (U.Symbol (src 1 3) (Unqualified "x")))]

    it "converts fn definition with single argument" $
      (snd <$> explodeTopLevel [FnDefinition
                                (src 1 1)
                                "f"
                                [Binding (src 1 2) "x"]
                                (Symbol (src 1 3) (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition
       (src 1 1)
       "f"
       Nothing
       (U.Fn
        (src 1 1)
        (U.Binding (src 1 2) "x")
        (U.Symbol (src 1 3) (Unqualified "x")))]

    it "converts fn definition with multiple arguments" $
      (snd <$> explodeTopLevel [FnDefinition
                                (src 1 1)
                                "f"
                                [Binding (src 1 2) "x", Binding (src 1 3) "y"]
                                (Symbol (src 1 4) (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition
       (src 1 1)
       "f"
       Nothing
       (U.Fn
        (src 1 1)
        (U.Binding (src 1 2) "x")
        (U.Fn
         (src 1 1)
         (U.Binding (src 1 3) "y")
         (U.Symbol (src 1 4) (Unqualified "x"))))]

    it "converts type signature with uncurried fn type expression" $
      (snd <$> explodeTopLevel [TypeSignature
                                (src 1 1)
                                "f"
                                (Implicit
                                 (src 1 2)
                                 (TEFn
                                  (src 1 2)
                                  (TECon (src 1 2) "a")
                                  [TECon (src 1 3) "a", TECon (src 1 4) "a"])),
                                FnDefinition
                                (src 2 1)
                                "f"
                                []
                                (Symbol (src 2 2) (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition
       (src 2 1)
       "f"
       (Just $ Forall (src 1 2) [] (TFn (src 1 2) (TCon (src 1 2) "a") (TFn (src 1 2) (TCon (src 1 3) "a") (TCon (src 1 4) "a"))))
       (U.NoArgFn (src 2 1) (U.Symbol (src 2 2) (Unqualified "x")))]

    it "converts type signature and definition" $
      (snd <$> explodeTopLevel [TypeSignature (src 1 1) "f" (Implicit (src 1 2) (TECon (src 1 2) "a")),
                                FnDefinition (src 2 1) "f" [] (Symbol (src 2 2) (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition
       (src 2 1)
       "f"
       (Just $ Forall (src 1 2) [] (TCon (src 1 2) "a"))
       (U.NoArgFn (src 2 1) (U.Symbol (src 2 2) (Unqualified "x")))]

    it "returns error on type signature without definition" $
      explodeTopLevel [TypeSignature
                       (src 1 1)
                       "f"
                       (Implicit (src 1 2) (TECon (src 1 2) "a"))]
      `shouldFailWith`
      [TypeSignatureWithoutDefinition
       (src 1 1)
       "f"
       (Forall (src 1 2) [] (TCon (src 1 2) "a"))]
