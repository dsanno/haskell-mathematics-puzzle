module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "countAlive" $ do
    it "should be 0 for 0, 1" $
      countAlive 0 1 `shouldBe` 0

    it "should be 1 for 1, 1" $
      countAlive 1 1 `shouldBe` 1

    it "should be 2 for 2, 1" $
      countAlive 2 1 `shouldBe` 2

    it "should be 6 for 1, 4" $
      countAlive 1 4 `shouldBe` 6
