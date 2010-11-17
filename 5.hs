myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' xs = foldl func [] xs where
    func xs x = x:xs
