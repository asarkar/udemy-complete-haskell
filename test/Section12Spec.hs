{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Section12Spec (spec) where

import Section12
  ( Tree (..),
    breadthFirst,
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

draw :: Tree Int -> [String]
draw Empty = []
draw (Node x l r) = show x : drawSubTrees [l, r]
  where
    drawSubTrees [] = []
    drawSubTrees [Empty] = []
    drawSubTrees [t] =
      "|" : shift "`- " "   " (draw t)
    drawSubTrees (Empty : ts) = drawSubTrees ts
    drawSubTrees (t : ts) =
      "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

drawTree :: Tree Int -> String
drawTree = unlines . draw

leaf :: Int -> Tree Int
leaf x = Node x Empty Empty

t1 :: Tree Int
t1 = Node 5 (Node 3 (Node 2 (leaf 1) Empty) (leaf 4)) (leaf 6)

t2 :: Tree Int
t2 = Node 1 (Node 2 (leaf 4) (leaf 5)) (Node 3 (leaf 6) (leaf 7))

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
      let t3 = Node 5 (leaf 6) (Node 3 (leaf 4) (Node 2 Empty (leaf 1)))
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
