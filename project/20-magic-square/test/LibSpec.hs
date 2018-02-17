module LibSpec (spec) where

import           Test.Hspec

import qualified Data.Map.Lazy as Map
import           Lib


spec :: Spec
spec =
  describe "countSubBySum" $ do
    it "should be Map.empty for []" $
      countSubBySum [] `shouldBe` Map.empty

    it "should have single element whose value is 1 for single element list" $
      countSubBySum [1] `shouldBe` Map.singleton 1 1

    it "should be map of (sum of sub list, sub lists count for that sum)" $
      countSubBySum [1, 1, 2] `shouldBe` Map.fromList [(1, 2), (2, 2), (3, 2), (4, 1)]
