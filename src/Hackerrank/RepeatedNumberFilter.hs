--module Hackerrank.RepeatedNumberFilter where

import qualified Data.Map as Map
import Data.List (sort)

filterRepeatedNumbers' :: [Int] -> Map.Map Int Int -> [Int]
filterRepeatedNumbers' [] countMap = sort [k |(k,v) <- Map.toList countMap, v > 1 ]
filterRepeatedNumbers' (x:xs) numCountMap = filterRepeatedNumbers' xs newNumCountMap where
  newNumCountMap = Map.insertWith (+) x 1 numCountMap

filterRepeatedNumbers :: [Int] -> [Int]
filterRepeatedNumbers listOfNumbers = filterRepeatedNumbers' listOfNumbers Map.empty

main = do
  putStrLn "Type a serie of numbers:"
  rawNumbers <- getLine
  let numbers = map read (words rawNumbers) :: [Int]
  putStrLn . show $ filterRepeatedNumbers numbers
