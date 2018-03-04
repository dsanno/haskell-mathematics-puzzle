module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "+" $
    it "adds two numbers" $
      (+) 1 2 `shouldBe` 3
