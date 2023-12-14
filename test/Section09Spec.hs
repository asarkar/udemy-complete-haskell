module Section09Spec (spec) where

import Data.Char (isSpace)
import Section09
  ( combined,
    consecutive,
    countIf,
    eql,
    filterFoldl,
    firstWord,
    flatten,
    myLength,
    myReverse,
    powersOf2,
    prod,
    prodEvens,
    scalarProduct,
  )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "eql" $ do
    prop "tells whether two lists of integers are equal" $
      \(xs, ys) -> eql (xs :: [Int]) ys `shouldBe` (xs == ys)

  describe "prod" $ do
    prop "returns the product of a list of integers" $
      \xs -> prod (xs :: [Int]) `shouldBe` product xs

  describe "prodEvens" $ do
    prop "product of all even numbers of a list of integers" $
      \xs -> prodEvens (xs :: [Int]) `shouldBe` (product . filter even) xs

  describe "powersOf2" $ do
    it "generates the list of all the powers of 2" $ do
      take 5 powersOf2 `shouldBe` [1, 2, 4, 8, 16]

  describe "scalarProduct" $ do
    it "returns the dot product of two lists" $ do
      scalarProduct [2.0, 1.0, 5.0] [3.0, 2.0, 2.0] `shouldBe` 18.0
      scalarProduct [3.0, 4.0] [5.0, 3.0] `shouldBe` 27.0

  describe "flatten" $ do
    prop "flattens a list of lists" $
      \xs -> flatten (xs :: [[Int]]) `shouldBe` concat xs

  describe "myLength" $ do
    prop "returns the length of a list" $
      \xs -> myLength (xs :: String) `shouldBe` length xs

  describe "myReverse" $ do
    prop "reverses a list" $
      \xs -> myReverse (xs :: [Int]) `shouldBe` reverse xs

  describe "firstWord" $ do
    prop "returns the first word of a string" $
      forAll genAlphaStrWithSpaces $
        \xs -> firstWord xs `shouldBe` (head . words) xs

  describe "countIf" $ do
    it "returns number of elements in the list that satisfy the predicate" $ do
      countIf (> 5) [1 :: Int .. 10] `shouldBe` 5
      countIf even [3 :: Int, 4, 6, 1] `shouldBe` 2

  describe "combined" $ do
    it "applies each function to the given list" $ do
      combined [1 :: Int, 2, 3] [(+ 1), (* 2), (^ (2 :: Int))]
        `shouldBe` [[2, 3, 4], [2, 4, 6], [1, 4, 9]]

  describe "consecutive" $ do
    it "applies each function, one after the other, to each element in the given list" $ do
      consecutive [1 :: Int, 2, 3] [(+ 1), (* 2), (^ (2 :: Int))]
        `shouldBe` [[2, 2, 1], [3, 4, 4], [4, 6, 9]]

  describe "filterFoldl" $ do
    it "returns a fold of all the elements that satisfy the given predicate" $ do
      filterFoldl even (*) 1 [4, 7, 2, 4, 9, 3] `shouldBe` (32 :: Int)

genAlphaStrWithSpaces :: Gen String
genAlphaStrWithSpaces = xs `suchThat` notEmpty
  where
    xs = listOf1 $ oneof [lowers, uppers, space]
    lowers = elements ['a' .. 'z']
    uppers = elements ['A' .. 'Z']
    space = return ' '
    trim = reverse . dropWhile isSpace
    strip = trim . trim
    notEmpty = not . null . strip
