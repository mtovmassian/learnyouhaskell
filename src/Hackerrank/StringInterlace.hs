module Hackerrank.StringInterlace where

interlace :: [Char] -> [Char] -> [Char]
interlace string1 string2 = concat [a:[b] | (a, b)<-zip string1 string2]