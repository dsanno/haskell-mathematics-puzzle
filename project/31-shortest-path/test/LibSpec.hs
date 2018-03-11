module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
  describe "allPaths" $ do
    it "returns list with empty list for 0 0" $
      allPaths 0 0 `shouldBe` [[]]

    it "returns single path for 1 0" $
      allPaths 1 0 `shouldBe` [[((0, 0), (1, 0))]]

    it "returns all paths" $ do
      let expected = [ [((0, 0), (0, 1)), ((0, 1), (1, 1)), ((1, 1), (2, 1))]
                     , [((0, 0), (1, 0)), ((1, 0), (2, 0)), ((2, 0), (2, 1))]
                     , [((0, 0), (1, 0)), ((1, 0), (1, 1)), ((1, 1), (2, 1))]
                     ]
      allPaths 2 1 `shouldMatchList` expected

  describe "countUnoverwrappedPaths" $
    it "returns 10 for 2 2" $
      countUnoverwrappedPaths 2 2 `shouldBe` 10
