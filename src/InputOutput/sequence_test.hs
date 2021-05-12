main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs
  sequence $ map print rs
  mapM print rs