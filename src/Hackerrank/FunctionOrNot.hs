module Hackerrank.FunctionOrNot where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

type DomainRangePairs = Map.Map Int [Int]
 
newDomainRangePairs :: [(Int, Int)] -> DomainRangePairs
newDomainRangePairs [] = Map.empty
newDomainRangePairs ((x,y):xs) = Map.insertWith (++) x [y] $ newDomainRangePairs xs
 
isValidFunction :: DomainRangePairs -> Bool
isValidFunction frd = not $ any (\x -> length x > 1) $ Map.elems frd

main = do
  numberOfTestCases <- getLine
  forM [1..read numberOfTestCases :: Int] (\_ -> do
    numberOfPairs <- getLine
    pairs <- forM [1..read numberOfPairs :: Int] (\_ -> do
      line <- getLine
      let [x, y] = map read $ words line :: [Int]
      return (x,y))
    let check = if isValidFunction (newDomainRangePairs pairs) then "YES" else "NO"
    putStrLn check)