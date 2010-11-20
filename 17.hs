split = flip $ splitAt

split' [] _ = ([], [])
split' xs 0 = ([], xs)
split' (x:xs) n =
    let (ys, zs) = split' xs (n-1)
    in (x:ys, zs)

---- I'm so stupid for not coming with this!
split'' xs n = (take n xs, drop n xs)
