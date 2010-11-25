--------
-- 31 --
--------

isPrime n = and $ [ n `mod` x /= 0 | x <- takeWhile (\x -> x*x <= n) [2..] ]

--------
-- 32 --
--------

myGCD x y = if x == 0 then y else myGCD (y `mod` x) x

--------
-- 33 --
--------

coprime x y
    | x >  y    = coprime y x
    | x == 0    = False
    | x == 1    = True
    | otherwise = coprime (y `mod` x) x

coprime' x y = (myGCD x y) == 1
