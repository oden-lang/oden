module Oden.Type.PolymorphicSpec where

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Test.Hspec

missing :: Metadata SourceInfo
missing = Metadata Missing

named :: String -> Type -> Type
named = TNamed missing . FQN [] . Identifier

typeInt :: Type
typeInt = TCon missing (nameInUniverse "int")

spec = do
  describe "underlying" $ do
    it "returns unnamed types as-is" $
      underlying typeInt `shouldBe` typeInt

    it "returns the underlying named type one level down" $
      underlying (named "A" typeInt) `shouldBe` typeInt

    it "returns the underlying named type one level down" $
      underlying (named "A" $ named "B" typeInt) `shouldBe` typeInt

  describe "rowToList" $ do
    it "return an empty list for an empty row" $
      rowToList (REmpty missing) `shouldBe` []

    it "return a one-element list for a single extension" $
      rowToList (RExtension missing (Identifier "foo") typeInt (REmpty missing)) `shouldBe` [(Identifier "foo", typeInt)]

    it "return a list with a pair for each extension" $
      rowToList (RExtension missing (Identifier "foo") typeInt (RExtension missing (Identifier "bar") typeInt (REmpty missing)))
      `shouldBe`
      [(Identifier "foo", typeInt),
       (Identifier "bar", typeInt)]

    it "ignore row variable" $
      rowToList (RExtension missing (Identifier "foo") typeInt (RExtension missing (Identifier "bar") typeInt (TVar missing (TV "a"))))
      `shouldBe`
      [(Identifier "foo", typeInt),
       (Identifier "bar", typeInt)]
