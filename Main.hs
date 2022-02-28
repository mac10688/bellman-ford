module Main where

import Prelude hiding (cycle)
import qualified Data.Map  as M
import Data.Maybe
import Control.Monad

data Edge = Edge {
    source :: Vertex,
    destination :: Vertex,
    weight :: Int
} deriving (Show)

type Vertex = String
type Predecessor = Vertex

data Distance = Infinity | DistanceValue Int

instance Show Distance where
    show (Infinity) = "Infinity"
    show (DistanceValue v) = show v

type DistanceMap = M.Map Vertex Distance
type PredecessorMap = M.Map Vertex Vertex

bellmanFord :: [Vertex] -> [Edge] -> Vertex -> [(DistanceMap, PredecessorMap)]
bellmanFord vertices edges source = 
    let
        distanceMap = M.fromList $ map (\v -> if v == source then (v, DistanceValue 0) else (v, Infinity)) vertices
        predecessorMap = M.empty
    in
        scanl 
            (\(distanceMap', predecessorMap') _ -> cycle edges distanceMap' predecessorMap') 
            (distanceMap, predecessorMap) 
            (replicate ((length vertices) - 1) ())

cycle :: [Edge] -> DistanceMap -> PredecessorMap -> (DistanceMap, PredecessorMap)
cycle [] distanceMap predecessorMap = (distanceMap, predecessorMap)
cycle (edge:edges) distanceMap predecessorMap =
    let
        w = weight edge
        u = source edge
        v = destination edge
        (newDistanceMap, newPredessorMap) = 
            case (lookupValue u, lookupValue v)  of
            (Infinity, _) -> (distanceMap, predecessorMap)
            (DistanceValue uValue, Infinity) -> (M.insert v (DistanceValue $ uValue + w) distanceMap, M.insert v u predecessorMap)
            (DistanceValue uValue, DistanceValue vValue) -> 
                                if (uValue + w < vValue) then 
                                    (M.insert v (DistanceValue (uValue + w)) distanceMap, M.insert v u predecessorMap) 
                                else 
                                    (distanceMap, predecessorMap)
    in
        cycle edges newDistanceMap newPredessorMap
    where
        lookupValue v = fromMaybe Infinity (M.lookup v distanceMap)



main :: IO ()
main = let
        vertices = ["A", "B", "C", "D", "E", "F", "G", "H"]
        edges = [
                    Edge {source="D", destination="E", weight=7}
                ,   Edge {source="D", destination="F", weight=10}
                ,   Edge {source="E", destination="B", weight=1}
                ,   Edge {source="E", destination="D", weight=7}
                ,   Edge {source="E", destination="F", weight=1}
                ,   Edge {source="F", destination="B", weight=3}
                ,   Edge {source="F", destination="D", weight=10}
                ,   Edge {source="F", destination="E", weight=1}
                ,   Edge {source="F", destination="G", weight=3}
                ,   Edge {source="F", destination="H", weight=1}
                ,   Edge {source="B", destination="A", weight=14}
                ,   Edge {source="B", destination="C", weight=8}
                ,   Edge {source="B", destination="E", weight=1}
                ,   Edge {source="B", destination="F", weight=3}
                ,   Edge {source="C", destination="A", weight=2}
                ,   Edge {source="C", destination="B", weight=8}
                ,   Edge {source="C", destination="G", weight=1}
                

                ,   Edge {source="G", destination="C", weight=1}
                ,   Edge {source="G", destination="F", weight=3}
                ,   Edge {source="G", destination="H", weight=1}
                ,   Edge {source="H", destination="F", weight=1}
                ,   Edge {source="H", destination="G", weight=5}
                ,   Edge {source="A", destination="B", weight=14}
                ,   Edge {source="A", destination="C", weight=2}
            ]
        source = "D"
    in
        forM_ (bellmanFord vertices edges source) (putStrLn . show)
