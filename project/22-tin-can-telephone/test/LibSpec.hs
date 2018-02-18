module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "countPatterns" $ do
    it "should be 0 for 0" $
      countPatterns 0 `shouldBe` 0

    it "should be 0 for 1" $
      countPatterns 1 `shouldBe` 0

    it "should be 1 for 2" $
      countPatterns 2 `shouldBe` 1

    it "should be the number of tin can telephone patterns" $
      countPatterns 6 `shouldBe` 5
