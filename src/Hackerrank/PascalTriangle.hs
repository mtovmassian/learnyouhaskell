module Hackerrank.PascalTriangle where

import Data.List

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

findBinomCoeff :: Double -> Double -> Integer
findBinomCoeff n r = round $ nFactorial / (rFactorial * nrDiffFactorial)
  where
    nFactorial = factorial n
    rFactorial = factorial r
    nrDiffFactorial = factorial (n - r)

findAllBinomCoeffs :: Double -> [[Integer]]
findAllBinomCoeffs kRows = [ map (findBinomCoeff rowIndex) [0..rowIndex] | rowIndex<-[0..(kRows - 1)]]

drawPascalTriangle :: Double -> [Char]
drawPascalTriangle kRows = pascalTriangleString
  where
    pascalTriangleString = intercalate "\n" spacedBinomCoeffs
    spacedBinomCoeffs = [unwords row | row<-stringifiedBinomCoeffs]
    stringifiedBinomCoeffs = [ map show row | row<-binomCoeffs]
    binomCoeffs = findAllBinomCoeffs kRows
 