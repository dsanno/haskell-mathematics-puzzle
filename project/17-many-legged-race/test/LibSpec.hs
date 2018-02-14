module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "countRowPatterns" $ do
    it "returns 2 for 1" $
      countRowPatterns 1 `shouldBe` 2

    it "returns 8 for 4" $
      countRowPatterns 4 `shouldBe` 8
