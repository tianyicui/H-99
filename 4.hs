myLength []     = 0
myLength (_:xs) = 1 + (myLength xs)

myLength' xs = foldl addOne 0 xs where
  addOne x _ = 1 + x

---- Learned lambuda, and the xs parameter is not necessary

myLength''  = foldr (\_ n -> n + 1) 0
myLength''' = foldl (\n _ -> n + 1) 0
