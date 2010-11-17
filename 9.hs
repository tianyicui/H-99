pack (x:xs) =
    let (a, b) = (dropAndConcat x xs)
    in a:(pack b) where
    dropAndConcat x xs
        | null xs || x /= (head xs) = ([x], xs)
        | otherwise                 = ([x]++c, d)
            where (c, d) = dropAndConcat x (tail xs)
pack [] = []

pack' :: (Eq a) => [a] -> [[a]]
pack' = foldr func []
    where
    func x []     = [[x]]
    func x (y:xs)
        | null y || x /= (head y) = ([x]:y:xs)
        | otherwise               = ((x:y):xs)
