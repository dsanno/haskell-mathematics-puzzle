module LibSpec (spec) where

import Test.Hspec

import qualified Data.Set as Set

import Lib


spec :: Spec
spec = do
  describe "hishaMoves" $ do
    context "without other piece" $
      it "shouldBe set of hisha moves" $ do
        let expected = Set.fromList [(0, 1), (1, 0), (2, 1), (1, 2)]
        hishaMoves 3 Set.empty (1, 1) `shouldBe` expected

    context "with other piece interference" $
      it "shouldBe set of hisha moves" $
        hishaMoves 3 (Set.fromList [(0, 1), (2, 0)]) (0, 0) `shouldBe` Set.fromList [(1, 0)]

  describe "kakuMoves" $ do
    context "without other piece" $
      it "shouldBe set of hisha moves" $ do
        let expected = Set.fromList [(0, 0), (2, 0), (0, 2), (2, 2)]
        kakuMoves 3 Set.empty (1, 1) `shouldBe` expected

    context "with other piece interference" $
      it "shouldBe set of hisha moves" $ do
        let expected = Set.fromList [(3, 1)]
        kakuMoves 5 (Set.fromList [(1, 1), (4, 2)]) (2, 0) `shouldBe` expected
