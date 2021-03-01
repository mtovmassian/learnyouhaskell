module Hackerrank.StringOPermute where

import Control.Monad (replicateM)
import Data.List

swapEvenChars :: [Char] -> [Char]
swapEvenChars string = concat reversedCharPairs
  where
    reversedCharPairs = map reverse charPairs
    charPairs = [take 2 $ drop charPairIndex string | charPairIndex<-charPairIndexes]
    charPairIndexes = filter even [0..(stringLength - 1)]
    stringLength = foldl (\acc _ -> acc + 1) 0 string
    
swapEvenChars' :: [Char] -> [Char]
swapEvenChars' [] = []
swapEvenChars' [x] = [x]
swapEvenChars' (x:y:xs) = y : x : swapEvenChars' xs

main :: IO ()
main = do
    numberOfTestCases <- readLn :: IO Int
    testCases <- replicateM numberOfTestCases getLine
    let results = map swapEvenChars testCases
    putStrLn $ intercalate "\n" results

--main = do
--        t <- readLn :: IO Int
--        inputdata <- getContents
--        putStr $ unlines $ map solve $ lines $ inputdata