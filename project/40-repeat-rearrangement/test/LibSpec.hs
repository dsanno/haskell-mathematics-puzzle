module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
  describe "tryRearrange" $ do
    it "returns Nothing if n'th position is other number" $
      tryRearrange 2 [Just 1, Just 3, Nothing] `shouldBe` Nothing

    it "returns Nothing for n to be other position" $
      tryRearrange 2 [Just 1, Nothing, Just 2] `shouldBe` Nothing

    it "reverses 1-n positions for n to be n's position" $
      tryRearrange 2 [Just 1, Just 2, Just 3] `shouldBe` Just [Just 2, Just 1, Just 3]

    it "reverses 1-n positions for Nothing to be n's position" $
      tryRearrange 2 [Just 1, Nothing, Just 3] `shouldBe` Just [Just 2, Just 1, Just 3]

  describe "findPath" $ do
    it "leaf for 1-depth tree" $
      search Leaf compare (+) (3 :: Int) `shouldBe` 3

    it "finds best path" $ do
      let expand x = if x <= 0
                     then Leaf 0
                     else Inner [0..(x - 1)]
      search expand compare (+) (3 :: Int) `shouldBe` 6
