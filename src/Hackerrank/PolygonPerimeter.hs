module Hackerrank.PolygonPerimeter where

import Control.Monad

data Point = Point Float Float deriving (Show)

data Edge = Edge Point Point deriving (Show)

getEdgeLength :: Edge -> Float
getEdgeLength (Edge (Point x1 y1) (Point x2 y2)) = distancePoint1ToPoint2 where
  distancePoint1ToPoint2 = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

applyShoelaceFormula :: [Edge] -> Float
applyShoelaceFormula edges_ = abs $ sum [(x1 * y2) - (y1 * x2) | (Edge (Point x1 y1) (Point x2 y2)) <- edges_] / 2

toEdges :: [Point] -> [Edge]
toEdges [] = []
toEdges [_] = []
toEdges [x, y] = [Edge x y]
toEdges (x:y:xs) = Edge x y:toEdges (y:xs)

data Polygon = Polygon {
  vertices :: [Point],
  edges :: [Edge]
} deriving (Show)


newPolygon :: [Point] -> Polygon
newPolygon [] = error "Poylgon is defined at least with 3 points."
newPolygon [_] = newPolygon []
newPolygon [_,_] = newPolygon []
newPolygon points = Polygon { vertices = points, edges = toEdges connectedPoints } where
  connectedPoints = points ++ [head points]

perimeter :: Polygon -> Float
perimeter Polygon {vertices = _, edges = edges_} = sum $ map getEdgeLength edges_

area :: Polygon -> Float
area Polygon {vertices = _, edges = edges_} = applyShoelaceFormula edges_

main = do
  numberOfPoints <- getLine
  points <- forM [1..read numberOfPoints :: Int] (\_ -> do
    line <- getLine
    let [coordX, coordY] = map read $ words line :: [Float]
    return (Point coordX coordY))
  print $ perimeter $ newPolygon points
