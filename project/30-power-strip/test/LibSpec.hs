module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
  describe "orderdCombinations" $
    it "returns ordered combinations whose  total is passed number" $ do
      orderdCombinations 1 1 1 `shouldMatchList` [[1]]
      orderdCombinations 2 1 4 `shouldMatchList` [[1, 3], [2, 2]]

  describe "countPatterns" $ do
    it "returns 2 for 3" $
      countPatterns 3 `shouldBe` 2
    it "returns 4 for 4" $
      countPatterns 4 `shouldBe` 4
