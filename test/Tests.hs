import Test.Tasty
import GraphTests
import DFSTests

tests :: TestTree
tests = testGroup "All tests" [graphTests, dfsTests]

main :: IO()
main = defaultMain tests