compress (x:y:xs) =
    (if x == y then [] else [x]) ++ compress (y:xs)
compress x        = x

compress' :: (Eq a) => [a] -> [a]
compress' = foldr func []
    where
    func x [] = [x]
    func x xs
        | x == head xs = xs
        | otherwise    = x:xs
