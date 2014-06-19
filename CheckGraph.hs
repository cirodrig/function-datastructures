
module CheckGraph where

import Data.Function
import Test.QuickCheck
import Set
import BoundedSet
import Graph

expected_bfs = [ [0]
               , [1,2,4,8]
               , [9,5,3,6,10,12]
               , [11,13,7,14]
               , [15]
               ]

computed_bfs = map boundedSetToList $ breadthFirstSearch (hypercube 4) 0

main =
  if ((==) `on` map sort) expected_bfs computed_bfs
  