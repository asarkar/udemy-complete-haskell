{-# LANGUAGE DerivingStrategies #-}

module Section12
  ( Tree (..),
    size,
    height,
    equal,
    isomorphic,
    preOrder,
    postOrder,
    inOrder,
    breadthFirst,
    breadthFirst',
  )
where

import qualified Data.Foldable as F
import qualified Data.Sequence as S

data Tree a = Node a (Tree a) (Tree a) | Empty deriving stock (Show)

-- Write a function size :: Tree a -> Int that, given a tree,
-- returns its size, that is, the number of nodes it contains.

size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

-- Write a function height :: Tree a -> Int that, given a tree,
-- returns its height, assuming that empty trees have zero height.

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- Write a function equal :: Eq a => Tree a -> Tree a -> Bool that,
-- given two trees, tells whether they are the same.

equal :: (Eq a) => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node x l1 r1) (Node y l2 r2) = x == y && equal l1 l2 && equal r1 r2

-- Write a function isomorphic Eq a => Tree a -> Tree a -> Bool that,
-- given two trees, tells whether they are isomorphic, that is, one
-- can be obtained from the other by flipping some of its descendants.

isomorphic :: (Eq a) => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node x l1 r1) (Node y l2 r2) = x == y && isomorphic l1 r2 && isomorphic r1 l2

-- Write a function preOrder :: Tree a -> [a] that,
-- given a tree, return its pre-order traversal.

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x l r) = [x] ++ preOrder l ++ preOrder r

-- Write a function postOrder :: Tree a -> [a] that,
-- given a tree, return its post-order traversal.

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x l r) = postOrder l ++ postOrder r ++ [x]

-- Write a function inOrder :: Tree a -> [a] that,
-- given a tree, return its in-order traversal.

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r

-- Write a function breadthFirst :: Tree a -> [a] that,
-- given a tree, return its traversal by levels.

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst root = reverse $ bfs (S.singleton root) []
  where
    updateQ :: S.Seq (Tree a) -> [Tree a] -> S.Seq (Tree a)
    updateQ = F.foldl go
      where
        go q' child = case child of
          Empty -> q'
          x -> q' S.|> x

    bfs :: S.Seq (Tree a) -> [a] -> [a]
    bfs q visited = case S.viewl q of
      ((Node x l r) S.:< rest) -> bfs (updateQ rest [l, r]) (x : visited)
      _ -> visited

breadthFirst' :: Tree a -> [a]
breadthFirst' = bfs . (: [])
  where
    bfs :: [Tree a] -> [a]
    bfs [] = []
    bfs (Empty : xs) = bfs xs
    bfs ((Node x l r) : xs) = x : bfs (xs ++ [l, r])
