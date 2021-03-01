module Hackerrank.StringCompression where

compress' :: [Char] -> Int -> [Char]
compress' [] _ = []
compress' [x] 1 = [x]
compress' [x] charReps = x : show charReps
compress' (x:xs) charReps
  | isCharSequenceOver = if charReps > 1 
    then (x : show charReps) ++ compress' xs 1
    else x : compress' xs 1
  | otherwise  = compress' xs (charReps + 1)
  where
    isCharSequenceOver = x /= head xs

compress :: [Char] -> [Char]
compress string = compress' string 1