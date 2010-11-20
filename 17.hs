split = flip $ splitAt

split' xs n
    | xs == [] = ([], [])
    | n  == 0  = ([], xs)
    | otherwise =
        let (a, b) = split' (tail xs) (n-1)
        in ((head xs):a, b)
