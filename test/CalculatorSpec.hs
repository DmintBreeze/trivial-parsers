module CalculatorSpec
  where

import Test.QuickCheck
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Calculator (parseExpr)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "test simple terms" $ do
    it "add 2 integers" $ property prop_add2
    it "add more than 2 integers" $ do
      parseExpr "1 + 2 + 3" `shouldBe` Right 6.0
    it "substract 2 integers" $ do
      parseExpr "2 - 1" `shouldBe` Right 1.0
    it "substract more than 2 integers" $ do
      parseExpr "3 - 2 - 1" `shouldBe` Right 0.0

-- only generate non-negative integers
int_gen :: Gen Int
int_gen = choose (0, 1000)

prop_add2 = forAll int_gen $ \x ->
            forAll int_gen $ \y ->
              parseExpr (show x ++ "+" ++ show y) == Right (fromIntegral (x + y))

