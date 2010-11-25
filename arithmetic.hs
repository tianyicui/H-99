--------
-- 31 --
--------

isPrime n = and $ [ n `mod` x /= 0 | x <- takeWhile (\x -> x*x <= n) [2..] ]
