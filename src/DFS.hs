module DFS where

import Graph ( Graph(edges) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State.Strict
import Control.Monad
import Control.DeepSeq (deepseq)


type OpApply a b = b -> a -> a

type OpSet vertex edge acc = (OpApply acc vertex, OpApply acc edge, OpApply acc edge, OpApply acc vertex)

type GraphTraverse vertex acc = State (acc, Set vertex)

dfsUpdate :: Ord vertex => vertex -> GraphTraverse vertex acc ()
dfsUpdate v = do
    (acc, checked) <- get
    Set.insert v checked `seq` acc `seq` put (acc, Set.insert v checked)  

dfsAccUpdate :: b -> OpApply acc b -> GraphTraverse vertex acc ()
dfsAccUpdate el f = do
    (acc, checked) <- get
    f el acc `seq` checked `seq` put (f el acc, checked)

dfsGenericMoveMonad :: Ord vertex => vertex -> OpSet vertex edge acc -> Graph vertex edge -> GraphTraverse vertex acc ()
dfsGenericMoveMonad currentVertex (onEnter, onGo, onReturn, onLeave) graph = do
    dfsUpdate currentVertex
    dfsAccUpdate currentVertex onEnter
    Map.foldrWithKey (\vNext e _ -> do
            (_, checked) <- get
            Control.Monad.when (Set.notMember vNext checked) $ do
                    dfsAccUpdate e onGo
                    dfsGenericMoveMonad vNext (onEnter, onGo, onReturn, onLeave) graph
                    dfsAccUpdate e onReturn) (return ()) currentNeighbors
    dfsAccUpdate currentVertex onLeave
        where
            currentNeighbors =
                    case Map.lookup currentVertex (edges graph) of
                        Nothing -> Map.empty
                        (Just x) -> x

runDfsGeneric :: (Ord vertex) => vertex -> OpSet vertex edge acc -> acc -> Graph vertex edge -> (acc, Set vertex)
runDfsGeneric vStart operators initAcc graph = execState (dfsGenericMoveMonad vStart operators graph) (initAcc, Set.empty)

dfsPath :: Ord vertex => vertex -> Graph vertex Int -> [vertex]
dfsPath v0 graph = fst $ runDfsGeneric v0 (ignore, ignore, ignore, addItem) [] graph
    where
        ignore _ y = y
        addItem x acc = x:acc