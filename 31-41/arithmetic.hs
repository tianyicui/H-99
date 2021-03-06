--------
-- 31 --
--------

isPrime n = and $ [ n `mod` x /= 0 | x <- takeWhile (\x -> x*x <= n) [2..] ]

----

-- use `all`
isPrime' n = all (\x -> n `mod` x /= 0) $ takeWhile (\x -> x*x <= n) [2..]

-- a great lazy solution
allPrimes = filter (isPrime) [2..]
isPrime'' n
    | n <  2 = False
    | n == 2 = True
    | n >  2 = all (\x -> n `mod` x /= 0)
             $ takeWhile (\x -> x*x <= n) allPrimes

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

coprime' x y = myGCD x y == 1

--------
-- 34 --
--------

totient n = length $ filter (coprime n) [1..n-1]

totient' 1 = 1
totient' n =
    let m = leastPrimeFactor n
        d = n `div` m
    in (*) (totient' d)
           (if d `mod` m == 0 then m else m - 1)

leastPrimeFactor n =
    head $ filter (\x -> n `mod` x == 0)
         $ (takeWhile (\x -> x*x <= n) allPrimes ++ [n])

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
        r * (m - 1) * m ^ (c - 1)

----

phi' n = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult n]

--------
-- 39 --
--------

primeR s t = filter isPrime [s..t]

---- sieve implemented beautifully
primes = sieve [2..] where
    sieve (n:ns) = n : sieve [ m | m <- ns, m `mod` n /= 0 ]
primeR' s t = takeWhile (<= t) $ dropWhile (< s) primes

--------
-- 40 --
--------

goldbach n =
    head [ (i, j) | i <- primeR 2 n, let j = n - i, isPrime j ]

--------
-- 41 --
--------

goldbachList s t = [ goldbach x | x <- [s..t], even x ]

goldbachList' s t l =
    filter (\(a,b) -> a > l && b > l)
    $ goldbachList s t
