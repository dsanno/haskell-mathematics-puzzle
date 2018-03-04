module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "countPaths" $
    it "counts paths" $
      countPaths 3 2 `shouldBe` 4
