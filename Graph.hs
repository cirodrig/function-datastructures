
module Graph where

import Data.Bits
import Test.QuickCheck
import Set
import BoundedSet

-- | A graph consists of vertices identified by integers in the range
--   from 0 to N and a mapping from each vertex to its set of neighbors.
data Graph = Graph Int (Int -> BoundedSet)

instance Arbitrary Graph where
  arbitrary = do n <- sized (\s -> choose (0, s+1))
                 adj <- arbitrary
                 return $ Graph n (BoundedSet n . adj)

instance Show Graph where
  show g = "(fromAdjacencyList " ++ show (adjacencyList g) ++ ")"

-- | Create a graph's adjacency list
adjacencyList :: Graph -> [(Int, Int)]
adjacencyList (Graph n adj) =
  [(i, j) | i <- [0..n], j <- boundedSetToList $ adj i]

-- | Create a graph from an adjacency list
fromAdjacencyList :: [(Int, Int)] -> Graph
fromAdjacencyList [] = Graph 0 (\_ -> bsEmpty)
fromAdjacencyList xs = let maxval = maximum [max i j | (i, j) <- xs]
                           neighbors i = [j | (i', j) <- xs, i == i']
                       in Graph maxval (boundedSetFromList . neighbors)

-- | Create a hypercube
hypercube :: Int -> Graph
hypercube n =
  Graph k (\i -> BoundedSet k (Set (\j -> popCount (i `xor` j) == 1)))
  where
    k = (1 `shiftL` n) - 1

-- | Get the number of vertices in a graph
graphSize :: Graph -> Int
graphSize (Graph n _) = n + 1   -- The range 0 through n has size n+1

-- | Get the neighbors of a graph vertex
neighbors :: Graph -> Int -> BoundedSet
neighbors (Graph n adj) i
  | i < 0 || i > n = bsEmpty
  | otherwise      = adj i

-- | @hasEdge g i j@ iff graph @g@ has an edge from @i@ to @j@
hasEdge :: Graph -> Int -> Int -> Bool
hasEdge g i j = j `bsMember` neighbors g i

-- | Perform a breadth-first search on the graph, starting from the given node.
--   Return a list holding the nodes at each depth.
breadthFirstSearch :: Graph -> Int -> [BoundedSet]
breadthFirstSearch _ _ = undefined
