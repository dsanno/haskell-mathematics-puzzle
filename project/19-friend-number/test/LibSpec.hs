module LibSpec (spec) where

import Test.Hspec

import Data.List as List
import Data.Set as Set
import Lib


spec :: Spec
spec = do
  describe "primes" $
    it "returns prime numbers list" $
      take 5 primes `shouldBe` [2, 3, 5, 7, 11]

  describe "factorize" $ do
    it "returns [] for 1" $
      factorize [] 1 `shouldBe` []

    it "returns input number for prime number" $
      factorize [2, 3] 11 `shouldBe` [11]

    it "returns factorized numbers for synthesis number" $
      factorize [2, 3] 12 `shouldMatchList` [2, 2, 3]

  describe "friends" $
    it "selects friend numbers" $ do
      let xs = Set.fromList [[3, 5], [3, 7]]
      friends [2, 5] xs `shouldBe` Set.fromList [[3, 5]]
