module MinPath where

import Graph

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Control.Monad.State.Strict
import Control.Monad

type MinPathTraverse vertex acc = State (acc, (Set vertex, Map Int vertex))

minPathUpdate :: (Ord vertex) => vertex -> Int -> MinPathTraverse vertex acc ()
minPathUpdate v w = do
    (acc, (checked, candidates)) <- get
    put (acc, (Set.insert v checked, Map.insert w v candidates))

bfsAccUpdate :: vertex -> (vertex -> acc-> acc) -> MinPathTraverse vertex acc ()
bfsAccUpdate el f = do
    (acc, aggr) <- get
    put (f el acc, aggr)

minPathPop :: (Ord vertex) => MinPathTraverse vertex acc ()
minPathPop = do
    (acc, (checked, candidates)) <- get
    put (acc, (checked, Map.deleteMin candidates))

bfsGenericMoveMonad :: Graph Int edge -> (Int -> acc -> acc)-> MinPathTraverse Int acc ()
bfsGenericMoveMonad graph f = do
    (_, (_, candidates)) <- get
    bfsAccUpdate (snd $ Map.findMin candidates) f
    if Map.null candidates
        then do
            return ()
        else do
            foldM_ (\_ (vNext, _) -> do
                    (_, (checked, _)) <- get
                    Control.Monad.when (Set.notMember vNext checked) $ do
                            minPathUpdate vNext (Set.size checked)
                            bfsGenericMoveMonad graph f) () (Map.toList (currentNeighbors (Map.findMin candidates)))
            minPathPop
                where
                    currentNeighbors minPair =
                            case Map.lookup (fst minPair) (edges graph) of
                                Nothing -> Map.empty
                                (Just x) -> x

runBfsGeneric :: Int -> (Int -> acc -> acc) -> acc -> Graph Int edge -> (acc, (Set Int, Map Int Int))
runBfsGeneric v0 f initAcc graph = execState (bfsGenericMoveMonad graph f) (initAcc, (Set.empty, Map.fromList [(0, v0)]))

bfsPath :: Int -> Graph Int edge -> [Int]
bfsPath v0 graph = fst $ runBfsGeneric v0 ad [] graph
    where
        ad x y = x: y