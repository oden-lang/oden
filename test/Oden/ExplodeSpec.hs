module Oden.ExplodeSpec where

import qualified Oden.Core.Untyped     as U
import           Oden.Explode
import           Oden.Identifier
import           Oden.Syntax
import           Oden.Type.Polymorphic
import           Test.Hspec

import           Oden.Assertions

spec :: Spec
spec = do
  describe "explodeExpr" $ do
    it "converts symbol" $
      explodeExpr (Symbol (Unqualified "x"))
      `shouldBe`
      U.Symbol (Unqualified "x")

    it "converts int literal" $
      explodeExpr (Literal (Int 1))
      `shouldBe`
      U.Literal (U.Int 1)

    it "converts bool literal" $
      explodeExpr (Literal (Bool True))
      `shouldBe`
      U.Literal (U.Bool True)

    it "converts fn application with no params" $
      explodeExpr (Application (Fn [] (Symbol (Unqualified "x"))) [])
      `shouldBe`
      U.Application (U.NoArgFn (U.Symbol (Unqualified "x"))) []

    it "converts fn application with multiple params" $
      explodeExpr (Application (Fn ["x", "y"] (Symbol (Unqualified "x"))) [Symbol (Unqualified "x"), Symbol (Unqualified "y")])
      `shouldBe`
      U.Application (U.Fn "x" (U.Fn "y" (U.Symbol (Unqualified "x")))) [U.Symbol (Unqualified "x"), U.Symbol (Unqualified "y")]

  describe "explodeTopLevel" $ do
    it "converts fn definition with no argument" $
      (snd <$> explodeTopLevel [FnDefinition "f" [] (Symbol (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition "f" Nothing (U.NoArgFn (U.Symbol (Unqualified "x")))]

    it "converts fn definition with single argument" $
      (snd <$> explodeTopLevel [FnDefinition "f" ["x"] (Symbol (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition "f" Nothing (U.Fn "x" (U.Symbol (Unqualified "x")))]

    it "converts fn definition with multiple arguments" $
      (snd <$> explodeTopLevel [FnDefinition "f" ["x", "y"] (Symbol (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition "f" Nothing (U.Fn "x" (U.Fn "y" (U.Symbol (Unqualified "x"))))]

    it "converts type signature with uncurried fn type expression" $
      (snd <$> explodeTopLevel [TypeSignature "f" (Implicit (TEFn (TECon "a") [TECon "a", TECon "a"])),
                                FnDefinition "f" [] (Symbol (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition "f" (Just $ Forall [] (TFn (TCon "a") (TFn (TCon "a") (TCon "a")))) (U.NoArgFn (U.Symbol (Unqualified "x")))]

    it "converts type signature and definition" $
      (snd <$> explodeTopLevel [TypeSignature "f" (Implicit (TECon "a")),
                                FnDefinition "f" [] (Symbol (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition "f" (Just $ Forall [] (TCon "a")) (U.NoArgFn (U.Symbol (Unqualified "x")))]

    it "returns error on type signature without definition" $
      explodeTopLevel [TypeSignature "f" (Implicit (TECon "a"))]
      `shouldFailWith`
      [TypeSignatureWithoutDefinition "f" (Forall [] (TCon "a"))]
