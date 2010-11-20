dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs) ++ dropEvery (drop n xs) n

---- using zip, seems like sth to avoid
dropEvery' xs n = map snd . filter ((n/=) . fst) . zip (cycle [1..n]) $ xs
