rotate xs n
    | null xs || n == 0 = xs
    | n > 0 = rotate ( (tail xs)++[head xs] ) ( n - 1 )
    | n < 0 = rotate ( (last xs): (init xs) ) ( n + 1 )

rotate' xs n
    | n >= 0 =
        let (fst, snd) = splitAt n xs
        in snd ++ fst
    | otherwise = rotate xs (n `mod` (length xs))
