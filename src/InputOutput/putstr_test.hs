putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr xs

main = do
  putStr "Hey, "
  putStr "I'm "
  putStr "Andy!"
