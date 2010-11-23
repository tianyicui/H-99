group []     [] = [[]]
group (n:ns) xs =
    concat $ map f $ comb n xs
    where
        f (x,y) = map (x:) $ group ns y

comb 0 list   = [([], list)]
comb n (x:xs) =
    (++)
    (map (\(h,t) -> (x:h, t)) $ comb (n-1) xs)
    (map (\(h,t) -> (h, x:t)) $ comb n xs)
comb _ _      = []
