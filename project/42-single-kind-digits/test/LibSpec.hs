module LibSpec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec =
  describe "show" $ do
    context "with Digits" $ do
      it "shows digits for single digit" $
        show (Digits [1]) `shouldBe` "1"

      it "shows digits for multiple digits" $
        show (Digits [1, 2, 3]) `shouldBe` "123"

    context "with Term" $ do
      it "shows digits for digits" $
        show (Number $ Digits [1, 2, 3]) `shouldBe` "123"

      it "shows multiplication terms concatenated with '*' for Mul" $ do
        let d12 = Number $ Digits [1, 2]
            d45 = Number $ Digits [4, 5]
        show (Mul d12 d45) `shouldBe` "12 * 45"

      it "shows division terms concatenated with '/' for Div" $ do
        let d12 = Number $ Digits [1, 2]
            d45 = Number $ Digits [4, 5]
        show (Div d12 d45) `shouldBe` "12 / 45"

    context "with Elem" $ do
      it "shows digits for digits" $
        show (Term $ Number $ Digits [1, 2, 3]) `shouldBe` "123"

      it "shows addition elements concatenated with '+' for Add" $ do
        let d12 = Term $ Number $ Digits [1, 2]
            d45 = Term $ Number $ Digits [4, 5]
        show (Add d12 d45) `shouldBe` "12 + 45"

      it "shows subtraction elements concatenated with '-' for Sub" $ do
        let d12 = Term $ Number $ Digits [1, 2]
            d45 = Term $ Number $ Digits [4, 5]
        show (Sub d12 d45) `shouldBe` "12 - 45"
