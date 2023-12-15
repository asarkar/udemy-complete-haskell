{-# LANGUAGE DerivingStrategies #-}

module Section14 (Tree (..), numnodes, stringToTree, pathLength, bottomUp) where

data Tree a = Node a [Tree a] deriving stock (Eq, Show)

-- Write a function numnodes :: Tree a -> Int that,
-- given a multiway tree, returns the number of nodes.

numnodes :: Tree a -> Int
numnodes (Node _ forest) = 1 + foldr ((+) . numnodes) 0 forest

-- Write a function stringToTree :: String -> Tree a that,
-- given a string, it builds the tree. We suppose that the
-- nodes of a multiway tree contain single characters.
-- In the depth-first order sequence of its nodes, a special
-- character ^ has been inserted whenever, during the tree traversal,
-- the move is a backtrack to the previous level.
--
-- Example:
--   The tree below is represented as: afg^^c^bd^e^^^
--             ┌──┐
--   ┌─────────┤a ├────────┐
--   │         └─┬┘        │
--   │           │         │
--   │           │         │
--  ┌┴─┐       ┌─┴┐       ┌┴─┐
--  │f │       │c │  ┌────┤b ├────┐
--  └┬─┘       └──┘  │    └──┘    │
--   │               │            │
--   │               │            │
--  ┌┴─┐           ┌─┴┐          ┌┴─┐
--  │g │           │d │          │e │
--  └──┘           └──┘          └──┘

stringToTree :: String -> Tree Char
stringToTree = head . fst . dfs
  where
    -- First recursive dfs call collects the children,
    -- second recursive dfs call collects the siblings.
    --
    -- x=g, xs=^^c^bd^e^^^, ys=^c^bd^e^^^, zs=c^bd^e^^^
    -- x=e, xs=^^^, ys=^^, zs=^
    -- x=d, xs=^e^^^, ys=e^^^, zs=^
    -- x=b, xs=d^e^^^, ys=^, zs=
    -- x=c, xs=^bd^e^^^, ys=bd^e^^^, zs=
    -- x=f, xs=g^^c^bd^e^^^, ys=c^bd^e^^^, zs=
    -- x=a, xs=fg^^c^bd^e^^^, ys=, zs=
    dfs :: String -> ([Tree Char], String)
    dfs [] = ([], [])
    dfs ('^' : xs) = ([], xs)
    dfs (x : xs) = (Node x children : siblings, zs)
      where
        (children, ys) = dfs xs
        (siblings, zs) = dfs ys

-- Write a function pathLength :: Tree a -> Int that,
-- given a multiway tree, it returns the path length
-- of the tree.
--
-- We define the path length of a multiway tree as the
-- total sum of the path lengths from the root to all
-- nodes of the tree.
--
-- Example:
--   The path length of the tree shown above is:
--   = g(2)+f(1)+c(1)+d(2)+e(2)+b(1)
--   = 9

pathLength :: Tree a -> Int
pathLength = dfs 0
  where
    dfs depth (Node _ forest) = depth + foldr ((+) . dfs (succ depth)) 0 forest

-- Write a function bottomUp :: Tree a -> [a] that constructs
-- the bottom-up sequence of the nodes of the multiway tree.
--
-- Example:
--   The bottom-up sequence of the tree shown above is: "gfcdeba".
--
-- Thus, the bottom-up sequence is basically post-order traversal.

bottomUp :: Tree a -> [a]
bottomUp (Node x forest) = concatMap bottomUp forest ++ [x]
