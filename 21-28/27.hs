group []     _  = [[]]
group (n:ns) xs =
    [ g:gs | (g,rs) <- comb n xs
           ,  gs    <- group ns rs ]

comb 0 xs     = [([], xs)]
comb n (x:xs) =
    (++)
    [ (x:h,t) | (h,t) <- comb (n-1) xs ]
    [ (h,x:t) | (h,t) <- comb  n    xs ]
comb _ _      = []

---- TODO: another implementation which is hard to understand
group' :: [Int] -> [a] -> [[[a]]]
group' [] = const [[]]
group' (n:ns) = concatMap (uncurry $ (. group' ns) . map . (:)) . comb n
