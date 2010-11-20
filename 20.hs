removeAt n (x:xs)
    | n == 1 = (x,xs)
    | otherwise =
      let (y,ys) = (removeAt (n-1) xs)
      in (y,x:ys)
