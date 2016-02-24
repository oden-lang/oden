module Oden.ExplodeSpec where

import qualified Oden.Core.Untyped     as U
import qualified Oden.Explode          as E
import           Oden.Explode          hiding (explodeTopLevel)
import           Oden.Identifier
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Syntax
import           Oden.Type.Signature
import           Test.Hspec

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<test>" l c)

explodeTopLevel = E.explodeTopLevel ["pkg"]

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
                    [NameBinding (src 1 2) "x",
                     NameBinding (src 1 3) "y"]
                    (Symbol (src 1 4) (Unqualified "x")))
                   [Symbol (src 1 5) (Unqualified "x"),
                    Symbol (src 1 9) (Unqualified "y")])
      `shouldBe`
      U.Application
      (src 1 1)
      (U.Fn
       (src 1 1)
       (U.NameBinding (src 1 2) "x")
       (U.Fn
        (src 1 1)
        (U.NameBinding (src 1 3) "y")
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
                                [NameBinding (src 1 2) "x"]
                                (Symbol (src 1 3) (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition
       (src 1 1)
       "f"
       Nothing
       (U.Fn
        (src 1 1)
        (U.NameBinding (src 1 2) "x")
        (U.Symbol (src 1 3) (Unqualified "x")))]

    it "converts fn definition with multiple arguments" $
      (snd <$> explodeTopLevel [FnDefinition
                                (src 1 1)
                                "f"
                                [NameBinding (src 1 2) "x", NameBinding (src 1 3) "y"]
                                (Symbol (src 1 4) (Unqualified "x"))])
      `shouldSucceedWith`
      [U.Definition
       (src 1 1)
       "f"
       Nothing
       (U.Fn
        (src 1 1)
        (U.NameBinding (src 1 2) "x")
        (U.Fn
         (src 1 1)
         (U.NameBinding (src 1 3) "y")
         (U.Symbol (src 1 4) (Unqualified "x"))))]

    it "converts struct definition with type parameters" $
      (snd <$> explodeTopLevel [StructDefinition
                                (src 1 1)
                                "S"
                                [NameBinding (src 1 2) "t"]
                                [StructFieldExpr (src 1 3) "x" (TSSymbol (src 1 4) (Unqualified "t"))]])
      `shouldSucceedWith`
      [U.StructDefinition
       (src 1 1)
       (FQN ["pkg"] "S")
       [U.NameBinding (src 1 2) "t"]
       [U.StructField (src 1 3) "x" (TSSymbol (src 1 4) (Unqualified "t"))]]
