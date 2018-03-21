module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
  describe "intToBits" $ do
    it "returns [] for 0 length" $
      intToBits 0 7 `shouldBe` []

    it "returns bit list" $
      intToBits 4 7 `shouldBe` [1, 1, 1, 0]

  describe "bitsToInt" $ do
    it "returns Int value represented by input" $
      bitsToInt [1, 0, 1] `shouldBe` 5

  describe "symmetrize" $ do
    it "returns [] for []" $
      symmetrize [] `shouldBe` []
    it "returns Int list whose bits are symmetric" $
      symmetrize [0, 1] `shouldBe` [0, 1, 128, 0]
