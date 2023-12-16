module Section23Spec (spec) where

import qualified Data.List as L
import Section23
  ( avg,
    canGo3',
    cat,
    denominator,
    empty,
    fsmap,
    get,
    inside,
    maximum,
    moves,
    multEq,
    numerator,
    rational,
    selectFirst,
    set,
  )
import Test.Hspec
import Tree (fromList)
import Prelude hiding (maximum)

spec :: Spec
spec = do
  describe "fsmap" $ do
    it "returns function applications from left to right" $ do
      fsmap (3 :: Int) [(+ 2), (* 3), (+ 4)] `shouldBe` 19
      fsmap "e" [(++ "llo"), (:) 'h', (++ "!")] `shouldBe` "hello!"
      fsmap False [] `shouldBe` False

  describe "Rational" $ do
    it "returns the numerator" $ do
      numerator (rational 1 2) `shouldBe` 1
    it "returns the denominator" $ do
      denominator (rational 1 2) `shouldBe` 2
    it "returns the numerator simplified" $ do
      numerator (rational 2 4) `shouldBe` 1
    it "returns the denominator simplified" $ do
      denominator (rational 2 4) `shouldBe` 2
    it "displays in the format x/y" $ do
      show (rational 1 2) `shouldBe` "1/2"
    it "displays in the simplified format x/y" $ do
      show (rational 2 4) `shouldBe` "1/2"
    it "equates as expected" $ do
      rational 1 2 == rational 2 4 `shouldBe` True
    it "does not equate as expected" $ do
      rational 1 2 == rational 1 3 `shouldBe` False

  describe "multEq" $ do
    it "generates an infinite sequence by multiplying by the product of the input numbers" $ do
      take 6 (multEq 2 3) `shouldBe` [1, 6, 36, 216, 1296, 7776]
      take 5 (multEq 3 7) `shouldBe` [1, 21, 441, 9261, 194481]

  describe "selectFirst" $ do
    it "returns the elements of l1 that appear in l2 in a position strictly smaller than in l3" $ do
      selectFirst [] [] [] `shouldBe` []
      selectFirst [8, 4, 5, 6, 12, 1] [] [8, 6, 5, 4, 1] `shouldBe` []
      selectFirst [8, 4, 5, 6, 12, 1] [4, 5, 6, 2, 8, 12] [] `shouldBe` [8, 4, 5, 6, 12]
      selectFirst [8, 4, 5, 6, 12, 1] [4, 5, 6, 2, 8, 12] [8, 6, 5, 4, 1] `shouldBe` [4, 5, 12]

  describe "Symbol table" $ do
    it "can get and set values" $ do
      get (set empty "a" 1) "a" `shouldBe` Just (1 :: Int)
      get (set empty "a" 1) "b" `shouldBe` (Nothing :: Maybe Int)
      get (set (set empty "a" 1) "b" 2) "a" `shouldBe` Just (1 :: Int)
      get (set (set empty "a" 1) "b" 2) "b" `shouldBe` Just (2 :: Int)
      get (set (set empty "a" 1) "b" 2) "c" `shouldBe` (Nothing :: Maybe Int)
      get (set (set empty "a" 1) "a" 2) "a" `shouldBe` Just (2 :: Int)

  describe "Knight" $ do
    it "given a position tells if it is inside the board" $ do
      inside (4, 5) `shouldBe` True
      inside (0, 1) `shouldBe` False
      inside (4, 9) `shouldBe` False

    it "given a position returns the list of the next positions" $ do
      (L.sort . moves) (4, 5) `shouldBe` [(2, 4), (2, 6), (3, 3), (3, 7), (5, 3), (5, 7), (6, 4), (6, 6)]
      (L.sort . moves) (1, 1) `shouldBe` [(2, 3), (3, 2)]

    it "given two positions tells if the Knight can reach the destination in three moves" $ do
      canGo3' (1, 1) (4, 5) `shouldBe` True
      canGo3' (1, 1) (4, 6) `shouldBe` False

  describe "Binary tree folding" $ do
    it "gives its average" $ do
      avg (fromList read ["10", "20", "30"]) `shouldBe` 20.0
    it "concatenates all node values" $ do
      cat (fromList id ["my", "dog", "likes", "null", "null", "summer"]) `shouldBe` "my dog likes summer"
    it "returns the maximum value" $ do
      maximum (fromList head ["a", "c", "b"]) `shouldBe` 'c'
