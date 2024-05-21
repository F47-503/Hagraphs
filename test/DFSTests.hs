module DFSTests where

import Test.Tasty
import Test.Tasty.HUnit
import Graph
import DFS

dfsTests :: TestTree
dfsTests = testGroup "dfs tests"
  [ testCase "Test simple path" $
      dfsPath 1 (orientedGraph [(1, 2), (2, 3)]) @?= [1, 2, 3]
    , testCase "Test additional edges in oriented path" $
      dfsPath 1 (orientedGraph [(1, 2), (2, 3), (1, 3)]) @?= [1, 2, 3]
    , testCase "Test many edges in oriented path" $
      dfsPath 1 (orientedGraph [(1, 2), (2, 3), (1, 3), (3, 4), (4, 1), (4, 2)]) @?= [1, 2, 3, 4]
    , testCase "Test edges from 1 vertex in not oriented graph" $
      dfsPath 1 (notOrientedGraph [(1, 2), (2, 3), (3, 1), (3, 4)]) @?= [1, 2, 3, 4]
  ]
