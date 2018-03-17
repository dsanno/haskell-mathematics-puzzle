module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
  describe "step" $
    it "moves reversed head dice to tail" $ do
      step [1, 2, 3, 4, 5, 6] `shouldBe` [2, 3, 4, 5, 6, 6]
      step [6, 1, 2, 3, 4, 5] `shouldBe` [1, 6, 5, 4, 3, 2]

  describe "splitLoop" $ do
    it "returns empty list and loop steps for first element of loop" $ do
      let loop = [ [3, 4, 3, 4, 3, 4]
                 , [4, 3, 4, 4, 3, 4]
                 , [3, 4, 3, 4, 3, 3]
                 , [4, 3, 3, 4, 3, 4]
                 , [3, 4, 3, 4, 4, 3]
                 , [4, 4, 3, 4, 3, 4]
                 , [3, 4, 3, 3, 4, 3]
                 ]
      splitLoop [3, 4, 3, 4, 3, 4] `shouldBe` ([], loop)

    it "returns not loop steps and loop steps" $ do
      let notLoop = [ [1, 3, 2, 5, 6, 4]
                    , [3, 2, 5, 6, 4, 6]
                    ]
          loop = [ [6, 4, 6, 4, 5, 2]
                 , [1, 3, 1, 3, 2, 5]
                 , [3, 1, 3, 2, 5, 6]
                 , [2, 5, 6, 4, 6, 4]
                 ]
      splitLoop [1, 3, 2, 5, 6, 4] `shouldBe` (notLoop, loop)
