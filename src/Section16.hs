{-# LANGUAGE DerivingStrategies #-}

module Section16 (acyclicPaths) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

-- Write a function acyclicPaths :: Eq a => a -> a -> [(a, a)] -> [[a]]
-- that given two nodes a and b in a graph, returns all the acyclic
-- paths from a to b.

acyclicPaths :: (Ord a) => a -> a -> [(a, a)] -> [[a]]
acyclicPaths src dest edges = dfs S.empty src
  where
    graph = foldr merge (M.empty :: Map a [a]) edges
    merge (k, v) = M.insertWith (++) k [v]

    dfs visited u
      | u == dest = [[u]]
      | otherwise = [u : xs | v <- neighbors, xs <- dfs visited' v, xs /= []]
      where
        visited' = S.insert u visited
        neighbors = filter (`S.notMember` visited') $ M.findWithDefault [] u graph

-- Write a function depthFirst :: Graph -> Node -> [Node] that generates a
-- depth-first order graph traversal sequence.
--
-- Use the following Graph Notation:
-- type Node = Int
-- type Edge = (Node, Node)
-- type Graph = ([Node], [Edge])

-- Write a predicate connectedcomponents :: Graph -> [[Node]]
-- that splits a graph into its connected components.
--
-- Use the following Graph Notation:
-- type Node = Int
-- type Edge = (Node, Node)
-- type Graph = ([Node], [Edge])
