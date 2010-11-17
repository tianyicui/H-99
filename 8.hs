compress (x:y:xs) =
    (if x == y then [] else [x]) ++ compress (y:xs)
compress x        = x

compress' :: (Eq a) => [a] -> [a]
compress' = foldr func []
    where
    func x (y:xs)
        | x == y    = (y:xs)
        | otherwise = (x:y:xs)
    func x []       = [x]
