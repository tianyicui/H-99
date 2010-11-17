compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = x:(compress (y:xs))
compress x      = x

compress' :: (Eq a) => [a] -> [a]
compress' = foldr func []
    where
    func x (y:xs)
        | x == y    = (y:xs)
        | otherwise = (x:y:xs)
    func x []       = [x]
