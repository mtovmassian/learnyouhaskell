import Control.Monad

main = do
   domains <- forM ["color", "animal", "number"] (\a -> do
    putStrLn $ "What is your favorite " ++ show a ++ "?"
    favorite <- getLine
    return favorite)
  putStrLn "Your favorites " ++ (unwords domains)


--foldr (\x y -> x ++ ", " ++ y) "" ["color", "animal", "number"]
