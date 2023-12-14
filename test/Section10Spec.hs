module Section10Spec (spec) where

import Section10 (factorials, fibs, hammings, ints, nats, ones, primes, triangulars)
import Test.Hspec

spec :: Spec
spec = do
  describe "ones" $ do
    it "generates an infinite sequence of ones" $ do
      take 8 ones `shouldBe` [1, 1, 1, 1, 1, 1, 1, 1]

  describe "nats" $ do
    it "generates an infinite sequence of natural numbers" $ do
      take 8 nats `shouldBe` [0 .. 7]

  describe "ints" $ do
    it "generates an infinite sequence of alternating numbers" $ do
      take 8 ints `shouldBe` [0, 1, -1, 2, -2, 3, -3, 4]

  describe "triangulars" $ do
    it "generates an infinite sequence of triangular numbers" $ do
      take 8 triangulars `shouldBe` [0, 1, 3, 6, 10, 15, 21, 28]

  describe "factorials" $ do
    it "generates an infinite sequence of factorials numbers" $ do
      take 8 factorials `shouldBe` [1, 1, 2, 6, 24, 120, 720, 5040]

  describe "fibs" $ do
    it "generates an infinite sequence of Fibonacci numbers" $ do
      take 8 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13]

  describe "primes" $ do
    it "generates an infinite sequence of prime numbers" $ do
      take 8 primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19]

  describe "hammings" $ do
    it "generates an infinite sequence of Hamming numbers" $ do
      take 8 hammings `shouldBe` [1, 2, 3, 4, 5, 6, 8, 9]
