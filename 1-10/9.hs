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
    where func x []     = [[x]]
          func x (y:xs) =
              if x == head y then (x:y):xs else [x]:y:xs

---- The use of `span`
pack'' (x:xs) =
    let (a,b) = span (==x) xs
    in (x:a) : pack'' b
pack'' [] = []
