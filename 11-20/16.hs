dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs) ++ dropEvery (drop n xs) n

---- using zip, seems like sth to avoid
dropEvery' = flip $ \n -> map snd . filter ((n/=) . fst) . zip (cycle [1..n])

---- rewritten in a fairly imperative way
dropEvery'' xs n = c where
    a = zip (cycle [1..n]) xs
    b = filter ((n/=) . fst) a
    c = map snd b
