elementAt (x:xs) n
    | n == 1    = x
    | otherwise = elementAt xs (n - 1)

elementAt' (x:_)  1 = x
elementAt' (_:xs) n = elementAt' xs (n - 1)
