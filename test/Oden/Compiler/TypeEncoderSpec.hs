module Oden.Compiler.TypeEncoderSpec where

import           Test.Hspec

import           Oden.Compiler.TypeEncoder
import           Oden.Identifier
import           Oden.Type.Monomorphic

spec :: Spec
spec =
  describe "encodeTypeInstance" $ do
    it "encodes simple type constructor" $
      encodeTypeInstance (Unqualified "foo") (TCon "Bar") `shouldBe` "foo_inst_Bar"
    it "encodes qualified type constructor" $
      encodeTypeInstance (Qualified "foo" "bar") (TCon "Baz") `shouldBe` "foo_bar_inst_Baz"
    it "encodes arrow" $
      encodeTypeInstance (Unqualified "foo") (TFn (TCon "Bar") (TCon "Baz")) `shouldBe` "foo_inst_Bar_to_Baz"
    it "encodes nested arrows" $
      encodeTypeInstance (Unqualified "foo") (TFn (TCon "Foo") (TFn (TCon "Bar") (TCon "Baz"))) `shouldBe` "foo_inst_Foo_to_Bar__to__Baz"
    it "encodes single arrow" $
      encodeTypeInstance (Unqualified "foo") (TNoArgFn (TCon "Bar")) `shouldBe` "foo_inst_to_Bar"
    it "encodes nested single arrows" $
      encodeTypeInstance (Unqualified "foo") (TNoArgFn (TNoArgFn (TCon "Bar"))) `shouldBe` "foo_inst_to_to__Bar"
    it "encodes uncurried func" $
      encodeTypeInstance (Unqualified "foo") (TUncurriedFn [TCon "Foo", TCon "Bar"] (TCon "Baz")) `shouldBe` "foo_inst_Foo_to_Bar_to_Baz"
    it "encodes variadic func" $
      encodeTypeInstance (Unqualified "foo") (TVariadicFn [TCon "Foo"] (TCon "Bar") (TCon "Baz")) `shouldBe` "foo_inst_Foo_to_variadic_Bar_to_Baz"
    it "encodes slice" $
      encodeTypeInstance (Unqualified "foo") (TFn (TSlice (TCon "Foo")) (TSlice (TCon "Bar"))) `shouldBe` "foo_inst_sliceof__Foo_to_sliceof__Bar"
