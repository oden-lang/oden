module Oden.Compiler.TypeEncoderSpec where

import           Test.Hspec

import           Oden.Compiler.TypeEncoder
import           Oden.Identifier
import           Oden.SourceInfo
import           Oden.Type.Monomorphic

spec :: Spec
spec =
  describe "encodeTypeInstance" $ do
    it "encodes simple type constructor" $
      encodeTypeInstance (Unqualified "foo") (TCon Missing "Bar") `shouldBe` "foo_inst_Bar"
    it "encodes qualified type constructor" $
      encodeTypeInstance (Qualified "foo" "bar") (TCon Missing "Baz") `shouldBe` "foo_bar_inst_Baz"
    it "encodes arrow" $
      encodeTypeInstance (Unqualified "foo") (TFn Missing (TCon Missing "Bar") (TCon Missing "Baz")) `shouldBe` "foo_inst_Bar_to_Baz"
    it "encodes nested arrows" $
      encodeTypeInstance (Unqualified "foo") (TFn Missing (TCon Missing "Foo") (TFn Missing (TCon Missing "Bar") (TCon Missing "Baz"))) `shouldBe` "foo_inst_Foo_to_Bar__to__Baz"
    it "encodes single arrow" $
      encodeTypeInstance (Unqualified "foo") (TNoArgFn Missing (TCon Missing "Bar")) `shouldBe` "foo_inst_to_Bar"
    it "encodes nested single arrows" $
      encodeTypeInstance (Unqualified "foo") (TNoArgFn Missing (TNoArgFn Missing (TCon Missing "Bar"))) `shouldBe` "foo_inst_to_to__Bar"
    it "encodes uncurried func" $
      encodeTypeInstance (Unqualified "foo") (TUncurriedFn Missing [TCon Missing "Foo", TCon Missing "Bar"] (TCon Missing "Baz")) `shouldBe` "foo_inst_Foo_to_Bar_to_Baz"
    it "encodes variadic func" $
      encodeTypeInstance (Unqualified "foo") (TVariadicFn Missing [TCon Missing "Foo"] (TCon Missing "Bar") (TCon Missing "Baz")) `shouldBe` "foo_inst_Foo_to_variadic_Bar_to_Baz"
    it "encodes slice" $
      encodeTypeInstance (Unqualified "foo") (TFn Missing (TSlice Missing (TCon Missing "Foo")) (TSlice Missing (TCon Missing "Bar"))) `shouldBe` "foo_inst_sliceof__Foo_to_sliceof__Bar"
