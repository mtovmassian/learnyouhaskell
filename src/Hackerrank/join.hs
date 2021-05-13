join :: [String] -> String -> String
join [] _ = ""
join (x:xs) separator  = foldl join_ x xs where
  join_ = \acc string -> acc ++ separator ++ string

--echo -e "a b c\n, " | runhaskell join.hs 
main = do
  raw_strings <- getLine
  separator <- getLine
  print $ join (words raw_strings) separator