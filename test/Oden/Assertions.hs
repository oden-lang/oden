module Oden.Assertions where

import           Test.Hspec

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

shouldSucceed :: (Eq a, Show a, Show e) => Either e a -> Expectation
shouldSucceed  res = res `shouldSatisfy` isRight

shouldSucceedWith :: (Eq v, Show v, Show e) => Either e v -> v -> Expectation
(Left err)    `shouldSucceedWith` _         = expectationFailure . show $ err
(Right value) `shouldSucceedWith` expected  = value `shouldBe` expected

shouldFail :: (Eq a, Show a, Show e) => Either e a -> Expectation
shouldFail res = res `shouldSatisfy` isLeft

shouldFailWith :: (Eq a, Show a, Eq e, Show e) => Either e a -> e -> Expectation
res `shouldFailWith` err = res `shouldSatisfy` (== Left err)
