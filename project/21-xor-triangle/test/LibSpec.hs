module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "xorTriangle" $
    it "should be list of xor triangle rows" $
      take 4 xorTriangle `shouldBe` [[1], [1, 1], [1, 0, 1], [1, 1, 1, 1]]
