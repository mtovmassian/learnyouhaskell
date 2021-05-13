--module Hackerrank.RepeatedNumberFilter where

import Control.Monad
import qualified Data.Map as Map
import Data.List (sort)

countRepetitions:: Ord a => [a] -> Map.Map a Int -> [(a, Int)]
countRepetitions [] countMap = Map.toList countMap
countRepetitions (x:xs) numCountMap = countRepetitions xs newNumCountMap where
  newNumCountMap = Map.insertWith (+) x 1 numCountMap

filterRepeatedNumbers :: [Int] -> Int -> [Int]
filterRepeatedNumbers listOfNumbers repetitionCountMin = sort repeatedNumbers where
  repeatedNumbers = [k |(k,v) <- repetitions, v >= repetitionCountMin ]
  repetitions = countRepetitions listOfNumbers Map.empty

main = do
  numberOfTestCases <- getLine
  forM [1..read numberOfTestCases :: Int] (\_ -> do
    [listSize, repetitionCountMin] <- getLine
    print listSize
    print repetitionCountMin)
  
