module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "countUnoverwrappedPaths" $
    it "returns 10 for 2 2" $
      countUnoverwrappedPaths 2 2 `shouldBe` 10
