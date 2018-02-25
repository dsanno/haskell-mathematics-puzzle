module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
  describe "isCross" $ do
    it "shouldBe True for crossed positions" $ do
      isCross ((False, 0), (True, 1)) ((False, 1), (True, 0)) `shouldBe` True
      isCross ((False, 1), (True, 2)) ((True, 1), (False, 3)) `shouldBe` True

    it "shouldBe False for not crossed positions" $ do
      isCross ((False, 1), (True, 2)) ((False, 0), (True, 1)) `shouldBe` False
      isCross ((False, 1), (True, 2)) ((True, 3), (False, 2)) `shouldBe` False

  describe "countCross" $
    it "should return the number of cross" $ do
      countCross [ (False, 0)
                 , (True, 1)
                 , (False, 1)
                 , (True, 0) ] `shouldBe` 1
      countCross [ (False, 0)
                 , (True, 1)
                 , (False, 2)
                 , (True, 2)
                 , (False, 1)
                 , (True, 0) ] `shouldBe` 2
      countCross [ (False, 0)
                 , (True, 2)
                 , (False, 2)
                 , (True, 1)
                 , (False, 1)
                 , (True, 0) ] `shouldBe` 3
