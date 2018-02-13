module LibSpec (spec) where

import           Test.Hspec

import qualified Lib


spec :: Spec
spec = do
  describe "rectangles" $ do
    it "returns [] for 0" $
      Lib.rectangles 0 `shouldBe` []

    it "returns [] for odd number" $
      Lib.rectangles 1 `shouldBe` []

    it "returns width and height tuples of rectangles that have passed perimeter" $
      Lib.rectangles 8 `shouldMatchList` [(1, 3), (2, 2)]

  describe "pairs" $ do
    it "returns [] for []" $
      (Lib.pairs [] :: [(Int, Int)]) `shouldBe` ([] :: [(Int, Int)])

    it "returns pairs consists of input elements" $
      Lib.pairs [1, 2, 3] `shouldMatchList` [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)]

  describe "rectanglePairs" $ do
    context "invalid cases" $ do
      it "returns [] for 4n + 1" $
        Lib.rectanglePairs 1 `shouldBe` []

      it "returns [] for 4n + 2" $
        Lib.rectanglePairs 2 `shouldBe` []

      it "returns [] for 4n + 3" $
        Lib.rectanglePairs 3 `shouldBe` []

    context "valid cases" $
      it "returns rectangle pairs whose total area equals square of passed perimeter" $
        Lib.rectanglePairs 20 `shouldMatchList` [((1, 9), (2, 8))]
