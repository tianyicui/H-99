removeAt n (x:xs)
    | n == 1 = xs
    | otherwise = x:(removeAt (n-1) xs)
