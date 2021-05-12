putStrLn' :: String -> IO ()
putStrLn' string = putStr $ string ++ "\n"

main = do
  putStrLn' "Hello,"
  putStrLn' "world"