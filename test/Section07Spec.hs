module Section07Spec (spec) where

import Section07 (dupli, insertIn, myButLast, myLast)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "myLast" $ do
    prop "returns the last element of a list" $
      forAll (listOf1 arbitrary :: Gen [Int]) $
        \xs -> myLast xs `shouldBe` last xs

  describe "myButLast" $ do
    prop "returns the penultimate element of a list" $
      forAll ((listOf1 arbitrary :: Gen [Int]) `suchThat` ((>= 2) . length)) $
        \xs -> myButLast xs `shouldBe` (last . init) xs

  describe "dupli" $ do
    prop "duplicates the elements of a list" $
      \xs -> dupli (xs :: [Int]) `shouldBe` (xs >>= \x -> [x, x])

  describe "insertIn" $ do
    prop "inserts an element in a given position into a list" $
      forAll genInputForInsertIn $
        \(xs, x, i) ->
          insertIn x xs i
            `shouldBe` (let (left, right) = splitAt (i - 1) xs in left ++ [x] ++ right)

genInputForInsertIn :: Gen ([Int], Int, Int)
genInputForInsertIn = do
  xs <- listOf1 arbitrary :: Gen [Int]
  x <- arbitrary :: Gen Int
  i <- chooseInt (1, length xs)
  return (xs, x, i)
