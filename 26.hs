import Data.List(tails)

combinations 0 _      = [[]]
combinations _ []     = []
combinations n (x:xs) =
    (map (x:) $ combinations (n-1) xs)
    ++ combinations n xs

---- a solution using tails and list comprehension
combinations' 0 _  = [ [] ]
combinations' n xs = [ y:ys | y:xs' <- tails xs
                            , ys <- combinations' (n-1) xs']
