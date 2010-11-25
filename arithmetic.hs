--------
-- 31 --
--------

isPrime n = and $ [ n `mod` x /= 0 | x <- takeWhile (\x -> x*x <= n) [2..] ]

--------
-- 32 --
--------

myGCD x y = if x == 0 then y else myGCD (y `mod` x) x
