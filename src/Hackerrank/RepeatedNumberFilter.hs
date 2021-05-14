--module Hackerrank.RepeatedNumberFilter where

import Control.Monad
import qualified Data.Map as Map

countRepetitions:: Ord a => [a] -> Map.Map a Int -> [(a, Int)]
countRepetitions [] countMap = Map.toList countMap
countRepetitions (x:xs) numCountMap = countRepetitions xs newNumCountMap where
  newNumCountMap = Map.insertWith (+) x 1 numCountMap

filterRepeatedNumbers :: [Int] -> Int -> [Int]
filterRepeatedNumbers listOfNumbers repetitionCountMin = repeatedNumbers where
  repeatedNumbers = [k | (k,v) <- repetitions, v >= repetitionCountMin]
  repetitions = countRepetitions listOfNumbers Map.empty

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

main = do
  numberOfTestCases <- getLine
  forM [1..read numberOfTestCases :: Int] (\_ -> do
    inputA <- getLine
    inputB <- getLine
--    inputA <- return "1 2"
--    inputB <- return "1 2 2 3 3 3"
    let [_, repetitionCountMin] = map read (words inputA) :: [Int]
        numbers = map read (words inputB) :: [Int]
        repeatedNumbers = filterRepeatedNumbers numbers repetitionCountMin
        results = if null repeatedNumbers
          then "-1"
          else repeatedNumbersOrderByFirstOccurrence where
            repeatedNumbersOrderByFirstOccurrence = unwords $ [show x | x <- uniq numbers, x `elem` repeatedNumbers]
    putStrLn results)
