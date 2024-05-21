module Graph where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State.Strict
type GraphTraverse vertex acc = State (acc, Set vertex)

data Graph vertex edge = Graph
    { edges :: Map vertex (Map vertex edge)
    } deriving (Show, Eq)

emptyGraph :: Graph vertex edge
emptyGraph = Graph Map.empty

insertEdgeGeneric :: Ord vertex => Map vertex (Map vertex edge) -> (vertex, vertex, edge) -> Map vertex (Map vertex edge)
insertEdgeGeneric outerMap (v1, v2, e) = Map.alter (insertPairGeneric v2 e) v1 outerMap
        where 
            insertPairGeneric :: Ord vertex => vertex -> edge -> Maybe (Map vertex edge) -> Maybe (Map vertex edge)
            insertPairGeneric val e Nothing = Just (Map.singleton val e)
            insertPairGeneric val e (Just vertices) = Just (Map.insert val e vertices)

orientedGraphGeneric :: Ord vertex => [(vertex, vertex, edge)] -> Graph vertex edge
orientedGraphGeneric pairs = Graph $ foldl insertEdgeGeneric Map.empty pairs
 
notOrientedGraphGeneric :: Ord vertex => [(vertex, vertex, edge)] -> Graph vertex edge
notOrientedGraphGeneric pairs = Graph $ foldl (symmetrize insertEdgeGeneric) Map.empty pairs
        where
            symmetrize :: (b -> (a, a, c) -> b) -> b -> (a, a, c) -> b
            symmetrize f acc (x, y, e) = f (f acc (x, y, e)) (y, x, e)

orientedGraph :: Ord vertex => [(vertex, vertex)] -> Graph vertex Int
orientedGraph pairs = orientedGraphGeneric (map (\(x, y) -> (x, y, 1)) pairs)

notOrientedGraph :: Ord vertex => [(vertex, vertex)] -> Graph vertex Int
notOrientedGraph pairs = notOrientedGraphGeneric (map (\(x, y) -> (x, y, 1)) pairs)

