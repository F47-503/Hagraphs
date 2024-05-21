module BFSTests where

import Test.Tasty
import Test.Tasty.HUnit
import Graph
import MinPath

bfsTests :: TestTree
bfsTests = testGroup "bfs tests"
  [ testCase "Test simple path" $
      bfsPath 1 (orientedGraph [(1, 2), (2, 3)]) @?= [1, 2, 3]
    , testCase "Test additional edges in oriented path" $
      bfsPath 1 (orientedGraph [(1, 2), (2, 3), (1, 3)]) @?= [1, 2, 3]
    , testCase "Test many edges in oriented path" $
      bfsPath 1 (orientedGraph [(1, 2), (2, 3), (1, 3), (3, 4), (4, 1), (4, 2)]) @?= [1, 2, 3, 4]
    , testCase "Test edges from 1 vertex in not oriented graph" $
      bfsPath 1 (notOrientedGraph [(1, 2), (2, 3), (2, 4)]) @?= [1, 2, 4, 3]
    , testCase "Test edges form cycle in not oriented graph" $
      bfsPath 2 (notOrientedGraph [(1, 3), (2, 4), (1, 4), (2, 3)]) @?= [2, 3, 1, 4]
  ]
