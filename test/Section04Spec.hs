module Section04Spec (spec) where

import Section04 (absValue, fib, isPrime, power)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (NonNegative (..))

spec :: Spec
spec = do
  describe "absValue" $ do
    prop "works for all integers" $
      \x -> absValue x `shouldBe` (abs x :: Int)

  describe "power" $ do
    prop "works for all non-negative integers" $
      \(NonNegative x) (NonNegative n) -> power x n `shouldBe` (x ^ n)

  describe "isPrime" $ do
    it "identifies prime numbers" $ do
      let input = [1, 2, 3, 4, 5, 6, 11]
      let output = [False, True, True, False, True, False, True]
      all (\(i, prime) -> isPrime i == prime) (zip input output)
        `shouldBe` True

{- ORMOLU_DISABLE -}
  describe "fib" $ do
    it "returns the first 20 Fibonacci numbers" $ do
      map fib [0 .. 19] `shouldBe`
        [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 
        144, 233, 377, 610, 987, 1597, 2584, 4181]
{- ORMOLU_ENABLE -}
