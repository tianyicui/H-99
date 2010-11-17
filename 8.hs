compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = x:(compress (y:xs))
compress x      = x
