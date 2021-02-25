module Hackerrank.Exponential where

exp' :: (Fractional a, Integral t) => t -> a -> a
exp' 1 x = 1 + x
exp' n x = exp' (n - 1) x + (nthPower / nthFactorial)
  where
    nthPower = x ^^ n
    nthFactorial = fromIntegral $ product [1..n]

exp10 :: Double -> Double
exp10 = exp' 9
