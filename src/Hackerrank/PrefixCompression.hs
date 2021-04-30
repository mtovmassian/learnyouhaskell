module Hackerrank.PrefixCompression where

extractPrefix :: String -> String -> String
extractPrefix [] _ = []
extractPrefix _ [] = []
extractPrefix (x:xs) (y:ys)
  | x == y = x : extractPrefix xs ys
  | x /= y = []

main :: IO ()
main = do
    let stringA = "abcdefpr"
    let stringB = "abcqpr"
    let prefix = extractPrefix stringA stringB
    let substringA = drop (length prefix) stringA
    let substringB = drop (length prefix) stringB
    putStrLn $ concat [show $ length prefix, " ", prefix]
    putStrLn $ concat [show $ length substringA, " ", substringA]
    putStrLn $ concat [show $ length substringB, " ", substringB]
