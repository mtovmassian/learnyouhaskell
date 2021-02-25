module Hackerrank.PascalTriangle where

import Data.List

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

findBinomCoeff :: (RealFrac a, Enum a, Integral b) => a -> a -> b
findBinomCoeff n r = round $ nFactorial / (rFactorial * nrDiffFactorial)
  where
    nFactorial = factorial n
    rFactorial = factorial r
    nrDiffFactorial = factorial (n - r)

findAllBinomCoeffs :: (RealFrac a, Enum a) => a -> [[Integer]]
findAllBinomCoeffs kRows = [ map (findBinomCoeff rowIndex) [0..rowIndex] | rowIndex<-[0..(kRows - 1)]]

drawPascalTriangle :: (RealFrac a, Enum a) => a -> [Char]
drawPascalTriangle kRows = pascalTriangleString
  where
    pascalTriangleString = intercalate "\n" spacedRows
    spacedRows = [unwords row | row<-stringifiedBinomCoeffs]
    stringifiedBinomCoeffs = [ map show row | row<-binomCoeffs]
    binomCoeffs = findAllBinomCoeffs kRows