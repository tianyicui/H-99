dupli = concatMap (\x -> [x,x])

----

-- use list comprehension
dupli' xs = concat [[x,x] | x <- xs]

-- the direct solution
dupli'' [] = []
dupli'' (x:xs) = x:x:dupli' xs

-- the direct solution using foldr
dupli''' = foldr (\x xs -> x:x:xs) []

