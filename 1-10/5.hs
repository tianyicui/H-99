myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' xs = foldl func [] xs where
    func xs x = x:xs

----

-- the concise standard version
myReverse'' = foldl (flip (:)) []

myReverse''' = doReverse [] where
    doReverse reversed []     = reversed
    doReverse reversed (x:xs) = doReverse (x:reversed) xs
