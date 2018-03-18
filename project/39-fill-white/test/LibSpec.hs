module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "toggle" $ do
    context "with blank" $
      it "toggle same row and column" $
        toggle 0 (1, 1) `shouldBe` listToBits [1, 4, 5, 6, 7, 9, 13]

    context "with not blank" $
      it "toggle same row and column" $ do
        let bits = listToBits [1, 5, 7, 13]
        toggle bits (1, 1) `shouldBe` listToBits [4, 6, 9]
