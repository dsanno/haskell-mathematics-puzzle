module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
  describe "isPalindrome" $ do
    it "returns True for empty list" $
      isPalindrome ([] :: [Int]) `shouldBe` True

    it "returns True for single item list" $
      isPalindrome [1] `shouldBe` True

    it "returns True for palindrome list" $
      isPalindrome [1, 2, 3, 2, 1] `shouldBe` True

    it "returns False for not palindrome list" $
      isPalindrome [1, 2, 3, 4, 1] `shouldBe` False

  describe "intToList" $
    it "returns digits list for input number" $
      intToList 123 `shouldBe` [1, 2, 3]

  describe "toBinary" $ do
    it "returns 0 for 0" $
      toBinary 0 `shouldBe` 0

    it "returns binary expresion for input number" $
      toBinary 5 `shouldBe` 101
