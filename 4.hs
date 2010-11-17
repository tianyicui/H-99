myLength []     = 0
myLength (_:xs) = 1 + (myLength xs)

myLength' xs = foldl addOne 0 xs where
  addOne x _ = 1 + x
