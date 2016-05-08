module Oden.Compiler.TypeEncoderSpec where

import           Test.Hspec

import           Oden.Compiler.TypeEncoder
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Monomorphic

missing :: Metadata SourceInfo
missing = Metadata Missing

con = TCon missing . FQN [] . Identifier

spec :: Spec
spec = do
  describe "encodeTypeInstance" $ do
    it "encodes arrow" $
      encodeTypeInstance (Identifier "foo") (TFn missing (con "int") (con "string")) `shouldBe` "foo_inst_int_to_string"
    it "encodes nested arrows" $
      encodeTypeInstance (Identifier "foo") (TFn missing (con "bool") (TFn missing (con "int") (con "string"))) `shouldBe` "foo_inst_bool_to_int__to__string"
    it "encodes single arrow" $
      encodeTypeInstance (Identifier "foo") (TNoArgFn missing (con "int")) `shouldBe` "foo_inst_to_int"
    it "encodes nested single arrows" $
      encodeTypeInstance (Identifier "foo") (TNoArgFn missing (TNoArgFn missing (con "int"))) `shouldBe` "foo_inst_to_to__int"
    it "encodes variadic func" $
      encodeTypeInstance (Identifier "foo") (TForeignFn missing True [con "bool", con "int"] [con "string"]) `shouldBe` "foo_inst_bool_to_int_variadic_to_string"
    it "encodes variadic func with multiple return values" $
      encodeTypeInstance (Identifier "foo") (TForeignFn missing True [con "bool", con "int"] [con "string", con "int"]) `shouldBe` "foo_inst_bool_to_int_variadic_to_tupleof__string__int__"
    it "encodes slice" $
      encodeTypeInstance (Identifier "foo") (TFn missing (TSlice missing (con "bool")) (TSlice missing (con "int"))) `shouldBe` "foo_inst_sliceof__bool_to_sliceof__int"
  describe "encodeMethodInstance" $
    it "encodes arrow" $
      encodeMethodInstance
      (FQN ["ignored"] (Identifier "Foo"))
      (Identifier "bar")
      (TFn missing (con "int") (con "string"))
      `shouldBe`
      "Foo_method_bar_inst_int_to_string"
