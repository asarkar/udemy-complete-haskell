module Section14Spec (spec) where

import Section14 (Tree (..), bottomUp, numnodes, pathLength, stringToTree)
import Test.Hspec

t1 :: Tree Char
t1 =
  Node
    'a'
    [ Node 'f' [Node 'g' []],
      Node 'c' [],
      Node 'b' [Node 'd' [], Node 'e' []]
    ]

spec :: Spec
spec = do
  describe "numnodes" $ do
    it "returns the number of nodes in a multiway tree" $ do
      numnodes (Node 'a' [Node 'b' []]) `shouldBe` 2
      numnodes t1 `shouldBe` 7

  describe "stringToTree" $ do
    it "builds a multiway tree from a string" $ do
      let tree = stringToTree "afg^^c^bd^e^^^"
      tree `shouldBe` t1

  describe "pathLength" $ do
    it "returns the path length of a multiway tree" $ do
      pathLength t1 `shouldBe` 9

  describe "bottomUp" $ do
    it "returns the bottom-up sequence of a multiway tree" $ do
      bottomUp t1 `shouldBe` "gfcdeba"
