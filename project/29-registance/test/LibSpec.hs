module LibSpec (spec) where

import           Test.Hspec

import qualified Data.List  as List
import           Lib


spec :: Spec
spec = do
  describe "calculate" $ do
    it "returns Node value for Node" $
      calculate (Node 2.0) `shouldBe` 2.0

    it "returns Series sum for Series" $
      calculate (Series (Node 1.0) (Node 2.0)) `shouldBe` 3.0

    it "returns Parallel sum for Parallel" $
      calculate (Parallel (Node 1.0) (Node 2.0)) `shouldBe` 2.0 / 3

  describe "makePatterns" $ do
    it "returns single Node for n=1" $
      makePatterns 2.0 1 `shouldBe` [Node 2.0]

    it "returns all connection patterns" $ do
      let patterns = makePatterns 1.0 3
      length patterns `shouldBe` 8
      length (List.nub patterns) `shouldBe` length patterns
