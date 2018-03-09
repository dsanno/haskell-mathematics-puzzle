module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "add" $
    it "adds two numbers" $
      add 1 2 `shouldBe` 3
