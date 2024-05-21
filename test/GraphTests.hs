module GraphTests where

import Test.Tasty
import Test.Tasty.HUnit
import Graph
import qualified Data.Map as Map
--import Data.Map (Map)

graphTests :: TestTree
graphTests = testGroup "basic tests"
  [ testCase "Test single edge" $
      edges (orientedGraph [(1, 2)]) @?= Map.fromList [(1, Map.fromList [(2, 1)])]
    , testCase "Test multiple edges from one vertex" $
      edges (orientedGraph [(1, 2), (1, 3), (1, 5)]) @?= Map.fromList [(1, Map.fromList [(2, 1), (3, 1), (5, 1)])]
    , testCase "Test complete graph" $
      edges (orientedGraph [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]) @?= Map.fromList [(1, Map.fromList [(2, 1), (3, 1), (4, 1)])
                                                                                              , (2, Map.fromList [(3, 1), (4, 1)])
                                                                                              , (3, Map.fromList [(4, 1)])]
    , testCase "Test not oriented graph" $
      edges (notOrientedGraph [(1, 2)]) @?= Map.fromList [(1, Map.fromList [(2, 1)]), (2, Map.fromList [(1, 1)])]
    , testCase "Test not oriented graph large" $
      edges (notOrientedGraph [(1, 2), (1, 3), (3, 4)]) @?= Map.fromList [(1, Map.fromList [(2, 1), (3, 1)]), (2, Map.fromList [(1, 1)]), (4, Map.fromList [(3, 1)]), (3, Map.fromList [(1, 1), (4, 1)])]
    , testCase "Test weight edges" $
      edges (orientedGraphGeneric [(1, 2, 28), (1, 3, 13), (1, 5, 36)]) @?= Map.fromList [(1, Map.fromList [(2, 28), (3, 13), (5, 36)])]
    , testCase "Test weight edges" $
      edges (orientedGraphGeneric [(1, 2, 28), (1, 3, 13), (1, 5, 36)]) @?= Map.fromList [(1, Map.fromList [(2, 28), (3, 13), (5, 36)])]
  ]
