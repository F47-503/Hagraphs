import Test.Tasty
import GraphTests
import DFSTests
import BFSTests

tests :: TestTree
tests = testGroup "All tests" [graphTests, dfsTests, bfsTests]

main :: IO()
main = defaultMain tests