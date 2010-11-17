pack (x:xs) =
    let (a, b) = (dropAndConcat x xs)
    in a:(pack b) where
    dropAndConcat x xs
        | null xs || x /= (head xs) = ([x], xs)
        | otherwise                 = ([x]++c, d)
            where (c, d) = dropAndConcat x (tail xs)
pack [] = []
