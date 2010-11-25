group []     [] = [[]]
group (n:ns) xs =
    [ g:gs | (g,rs) <- comb n xs
           ,  gs    <- group ns rs ]

comb 0 xs     = [([], xs)]
comb n (x:xs) =
    (++)
    [ (x:h,t) | (h,t) <- comb (n-1) xs ]
    [ (h,x:t) | (h,t) <- comb  n    xs ]
comb _ _      = []
