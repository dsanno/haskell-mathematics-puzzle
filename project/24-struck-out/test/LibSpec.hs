module LibSpec (spec) where

import Test.Hspec

import qualified Data.Set as Set
import Lib


spec :: Spec
spec =
  describe "countPatterns" $ do
    it "should be 1 for empty set" $
      countPatterns [[1]] Set.empty `shouldBe` 1

    it "should be 1 for singleton set" $
      countPatterns [[1], [2]] (Set.singleton 1) `shouldBe` 1

    it "should count the number of ways to remove all numbers" $
      countPatterns [[1], [2], [1, 2]] (Set.fromList [1, 2]) `shouldBe` 3
