--module Hackerrank.RepeatedNumberFilter where

import Control.Monad
import qualified Data.Map as Map
import Data.List (sortOn)

enumerate :: [a] -> [(Int, a)]
enumerate list =
  let indexRange = [0..length list - 1]
  in zip indexRange list

data ItemOccurrence a = ItemOccurrence {
  item :: a,
  indexes :: [Int],
  numOfReps :: Int
} deriving (Ord, Eq, Show)

newItemOccurrence :: a -> Int -> ItemOccurrence a
newItemOccurrence item' index = ItemOccurrence { item = item', indexes = [index], numOfReps = 1 }

recordOccurrence :: ItemOccurrence a -> Int -> ItemOccurrence a
recordOccurrence ItemOccurrence {item = a, indexes = b, numOfReps = c} index = ItemOccurrence {
  item = a, indexes = [index] ++ b, numOfReps = c + 1
}

type OCounter a = Map.Map a (ItemOccurrence a)

countOccurrences' :: Ord a => [(Int, a)] -> OCounter a
countOccurrences' [] = Map.empty
countOccurrences' ((x,y):xs) = counter where
  counter = Map.insertWith (\ _ current -> recordOccurrence current x) y itemOccurrence $ countOccurrences' xs
  itemOccurrence = newItemOccurrence y x

countOccurrences :: Ord a => [a] -> [ItemOccurrence a]
countOccurrences iterable = Map.elems $ countOccurrences' $ enumerate iterable

firstOccurrenceIndex :: ItemOccurrence a -> Int
firstOccurrenceIndex itemOccurrence = foldr1 (\x y -> if x <= y then x else y) $ indexes itemOccurrence

sortByFirstIndex :: [ItemOccurrence a] -> [ItemOccurrence a]
sortByFirstIndex = sortOn firstOccurrenceIndex

filterRepeatedNumbers :: [Int] -> Int -> [Int]
filterRepeatedNumbers listOfNumbers repetitionCountMin = repeatedNumbers where
  repeatedNumbers = [item occurrence | occurrence <- occurrences, numOfReps occurrence >= repetitionCountMin]
  occurrences = sortByFirstIndex $ countOccurrences listOfNumbers

main = do
  numberOfTestCases <- getLine
  forM [1..read numberOfTestCases :: Int] (\_ -> do
    inputA <- getLine
    inputB <- getLine
    let [_, repetitionCountMin] = map read (words inputA) :: [Int]
        numbers = map read (words inputB) :: [Int]
        repeatedNumbers = filterRepeatedNumbers numbers repetitionCountMin
        results = if null repeatedNumbers
          then "-1"
          else unwords $ [show x | x <- repeatedNumbers]
    putStrLn results)