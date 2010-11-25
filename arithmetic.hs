import Data.List(find)

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

--------
-- 34 --
--------

totient n = length $ filter (coprime n) [1..n]

totient' 1 = 1
totient' n =
    let m = leastPrimeFactor
        d = n `div` m
    in (*) (totient' d)
           (if d `mod` m == 0 then m else m - 1)
    where
    leastPrimeFactor =
        head $ filter (\x -> n `mod` x == 0)
             $ (takeWhile (\x -> x*x <= n) [2..] ++ [n])
