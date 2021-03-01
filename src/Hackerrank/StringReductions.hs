module Hackerrank.StringReductions where

import Data.List (nub)

main = do
   testCase <- getContents
   putStrLn $ nub testCase