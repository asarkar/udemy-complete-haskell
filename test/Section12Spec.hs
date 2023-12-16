module Section12Spec (spec) where

import Section12
  ( breadthFirst,
    breadthFirst',
    equal,
    height,
    inOrder,
    isomorphic,
    postOrder,
    preOrder,
    size,
  )
import Test.Hspec
import Tree (Tree (..), fromList)

t1 :: Tree Int
t1 = fromList read ["5", "3", "6", "2", "4", "null", "null", "1"]

t2 :: Tree Int
t2 = fromList read ["1", "2", "3", "4", "5", "6", "7"]

spec :: Spec
spec = do
  describe "size" $ do
    it "returns the number of nodes in the tree" $ do
      size Empty `shouldBe` 0
      size t1 `shouldBe` 6

  describe "height" $ do
    it "returns the height of the tree" $ do
      height Empty `shouldBe` 0
      height t1 `shouldBe` 4

  describe "equal" $ do
    it "tells whether two trees are the same" $ do
      equal (Empty :: Tree Int) Empty `shouldBe` True
      equal Empty t1 `shouldBe` False
      equal t1 Empty `shouldBe` False
      equal t1 t1 `shouldBe` True
      equal t1 t2 `shouldBe` False

  describe "isomorphic" $ do
    it "tells whether two trees are isomorphic" $ do
      isomorphic (Empty :: Tree Int) Empty `shouldBe` True
      isomorphic Empty t1 `shouldBe` False
      isomorphic t1 Empty `shouldBe` False
      let t3 = fromList read ["5", "6", "3", "null", "null", "4", "2", "null", "null", "null", "1"]
      isomorphic t1 t3 `shouldBe` True
      isomorphic t1 t2 `shouldBe` False

  describe "preOrder" $ do
    it "returns the pre-order traversal of a tree" $ do
      preOrder t1 `shouldBe` [5, 3, 2, 1, 4, 6]
      preOrder t2 `shouldBe` [1, 2, 4, 5, 3, 6, 7]

  describe "postOrder" $ do
    it "returns the post-order traversal of a tree" $ do
      postOrder t1 `shouldBe` [1, 2, 4, 3, 6, 5]
      postOrder t2 `shouldBe` [4, 5, 2, 6, 7, 3, 1]

  describe "inOrder" $ do
    it "returns the in-order traversal of a tree" $ do
      inOrder t1 `shouldBe` [1, 2, 3, 4, 5, 6]
      inOrder t2 `shouldBe` [4, 2, 5, 1, 6, 3, 7]

  describe "breadthFirst" $ do
    it "returns the breadth-first traversal of a tree" $ do
      breadthFirst t1 `shouldBe` [5, 3, 6, 2, 4, 1]
      breadthFirst' t1 `shouldBe` [5, 3, 6, 2, 4, 1]
