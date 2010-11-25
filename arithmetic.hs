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
    let m = leastPrimeFactor n
        d = n `div` m
    in (*) (totient' d)
           (if d `mod` m == 0 then m else m - 1)

leastPrimeFactor n =
    head $ filter (\x -> n `mod` x == 0)
         $ (takeWhile (\x -> x*x <= n) [2..] ++ [n])

--------
-- 35 --
--------

primeFactors 1 = []
primeFactors n =
    eliminateFactor n where
    m = leastPrimeFactor n
    eliminateFactor x =
        if x `mod` m == 0
        then m:(eliminateFactor $ x `div` m)
        else primeFactors x

--------
-- 36 --
--------

primeFactorsMult 1 = []
primeFactorsMult n =
    let (r, c) = howManyFactor n
    in (m, c) : primeFactorsMult r
    where
        m = leastPrimeFactor n
        howManyFactor x =
            if x `mod` m == 0
            then let (r, c) = howManyFactor $ x `div` m
                 in  (r, c + 1)
            else (x, 0)

--------
-- 37 --
--------

phi n = foldr step 1 $ primeFactorsMult n where
    step (m, c) r =
        r * (m - 1) * ( foldr (*) 1 $ replicate (c - 1) m )
