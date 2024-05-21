module MinPath where

import Graph

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Control.Monad.State.Strict
import Control.Monad

type MinPathTraverse vertex acc weight = State (acc, (Set vertex, Map weight vertex))


minPathUpdate :: (Ord vertex, Ord weight) => vertex -> weight -> MinPathTraverse vertex acc weight ()
minPathUpdate v w = do
    (acc, (checked, candidates)) <- get
    put (acc, (Set.insert v checked, Map.insert w v candidates))

minPathPop :: (Ord vertex, Ord weight) => MinPathTraverse vertex acc weight ()
minPathPop = do
    (acc, (checked, candidates)) <- get
    if null candidates
        then do
            return ()
        else do
            put (acc, (checked, Map.deleteMin candidates))

bfsGenericMoveMonad :: (Ord vertex) => Graph vertex Int -> MinPathTraverse vertex acc Int ()
bfsGenericMoveMonad graph = do
    (acc, (_, structure)) <- get
    
    foldM_ (\_ (vNext, _) -> do
            (_, (checked, _)) <- get
            Control.Monad.when (Set.notMember vNext checked) $ do
                    minPathPop
                    minPathUpdate vNext (Set.size checked)
                    bfsGenericMoveMonad graph) () (Map.toList currentNeighbors)
        where
            minPair structure = if Map.null structure then 
            currentNeighbors =
                    case Map.lookup (fst minPair) (edges graph) of
                        Nothing -> Map.empty
                        (Just x) -> x

runDfsGeneric :: (Ord vertex, Ord weight) => vertex -> (vertex -> container vertex weight) -> acc -> Graph vertex edge -> (acc, Set vertex)
runDfsGeneric vStart operators initAcc graph = (initAcc, Set.empty)

dfsPath :: Ord vertex => vertex -> Graph vertex Int -> [vertex]
dfsPath v0 graph = [v0]
    where
        ignore _ y = y
        addItem x acc = x:acc